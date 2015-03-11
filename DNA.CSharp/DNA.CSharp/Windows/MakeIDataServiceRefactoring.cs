using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.Simplification;
using Microsoft.CodeAnalysis.CSharp.Extensions;

namespace DNA.CSharp.WPF
{
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(MakeIDataServiceRefactoring))]
    internal class MakeIDataServiceRefactoring : CodeRefactoringProvider
    {
        public async sealed override Task ComputeRefactoringsAsync(CodeRefactoringContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            // Find the node at the selection.
            var node = root.FindNode(context.Span);

            // Only offer a refactoring if the selected node is a class statement node.
            var classDecl = (InterfaceDeclarationSyntax)node;
            if (classDecl==null)
            {
                return;
            }

            var action = CodeAction.Create("Implement IDataService", c=> MakeIDataServiceAsync(context.Document, classDecl, c));

            // Register this code action.
            context.RegisterRefactoring(action);
        }

        private async Task<Document> MakeIDataServiceAsync(Document document, InterfaceDeclarationSyntax interfaceDeclaration, CancellationToken cancellationToken)
        {
            var newImplementation = @"
        void Add(object dataSource);
        void Delete(object dataSource);
        void Save(object dataSource);
        System.Linq.IQueryable<object> GetAllItems();
";

            var newInterfaceTree = SyntaxFactory.ParseSyntaxTree(newImplementation).
        GetRoot().DescendantNodes().
        Where(n=> n.IsKind(SyntaxKind.MethodDeclaration)).Cast<MethodDeclarationSyntax>().Select(m=> m.WithAdditionalAnnotations(Formatter.Annotation, Simplifier.Annotation)).ToArray();

            var newInterfaceBlock = SyntaxFactory.InterfaceDeclaration("IDataService").
                AddModifiers(SyntaxFactory.ParseToken("public")).WithOpenBraceToken(SyntaxFactory.ParseToken("{")).
                WithCloseBraceToken(SyntaxFactory.ParseToken("}").WithTrailingTrivia(SyntaxFactory.CarriageReturnLineFeed));

            var newInterfaceNode = newInterfaceBlock.AddMembers(newInterfaceTree).NormalizeWhitespace();

            var root = await document.GetSyntaxRootAsync();
            SyntaxNode newRoot = root.ReplaceNode(interfaceDeclaration, newInterfaceNode);

            var newDocument = document.WithSyntaxRoot(newRoot);

            return newDocument;
        }
    }

    //interface IDataService
    //{
    //    void Add(object dataSource);
    //    void Delete(object dataSource);
    //    void Save(object dataSource);
    //    IQueryable<object> GetAllItems();
    //}
}