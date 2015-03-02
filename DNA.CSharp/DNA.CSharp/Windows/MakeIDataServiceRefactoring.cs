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

        private async Task<Document> MakeIDataServiceAsync(Document document, InterfaceDeclarationSyntax classDeclaration, CancellationToken cancellationToken)
        {
            var newClass = @"    interface IDataService
    {
        void Add(object dataSource);
        void Delete(object dataSource);
        void Save(object dataSource);
        System.Linq.IQueryable<object> GetAllItems();
    }";

            var root = await document.GetSyntaxRootAsync();
            SyntaxNode newRoot = SyntaxFactory.ParseCompilationUnit(newClass);

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