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

namespace DNA.CSharp.Windows
{
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(MakeViewModelBaseRefactoring))]
    internal class MakeViewModelBaseRefactoring : CodeRefactoringProvider
    {
        public async sealed override Task ComputeRefactoringsAsync(CodeRefactoringContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            // Find the node at the selection.
            var node = root.FindNode(context.Span);

            // Only offer a refactoring if the selected node is a class statement node.
            var classDecl = node as ClassDeclarationSyntax;
            if (classDecl == null)
            {
                return;
            }
            var action = CodeAction.Create("Implement ViewModelBase", c => MakeViewModelBaseAsync(context.Document, classDecl, c));

            // Register this code action.
            context.RegisterRefactoring(action);
        }

        private async Task<Document> MakeViewModelBaseAsync(Document document, ClassDeclarationSyntax classDeclaration, CancellationToken cancellationToken)
        {

            var newImplementation = @"
	public event System.ComponentModel.PropertyChangedEventHandler PropertyChanged;

	//Raise a property change notification
	protected virtual void OnPropertyChanged(string propname)
	{
		if (PropertyChanged != null) {
			PropertyChanged(this, new System.ComponentModel.PropertyChangedEventArgs(propname));
		}
	}
";

            var newClassTree = SyntaxFactory.ParseSyntaxTree(newImplementation).
        GetRoot().DescendantNodes().
        Where(n => n.IsKind(SyntaxKind.FieldDeclaration) || n.IsKind(SyntaxKind.MethodDeclaration) || n.IsKind(SyntaxKind.PropertyDeclaration)
                || n.IsKind(SyntaxKind.ConstructorDeclaration) || n.IsKind(SyntaxKind.EventDeclaration) || n.IsKind(SyntaxKind.EventFieldDeclaration)).
        Cast<MemberDeclarationSyntax>().
        Select(decl => decl.WithAdditionalAnnotations(Formatter.Annotation, Simplifier.Annotation)).
        ToArray();


            ClassDeclarationSyntax newClassBlock = SyntaxFactory.ClassDeclaration("ViewModelBase").AddModifiers(SyntaxFactory.
                ParseToken("abstract")).WithOpenBraceToken(SyntaxFactory.ParseToken("{")).
                    WithCloseBraceToken(SyntaxFactory.ParseToken("}").WithTrailingTrivia(SyntaxFactory.CarriageReturnLineFeed));

            newClassBlock = newClassBlock.AddBaseListTypes(SyntaxFactory.SimpleBaseType(SyntaxFactory.
                ParseTypeName("System.ComponentModel.INotifyPropertyChanged")));

            var newClassNode = newClassBlock.AddMembers(newClassTree);

            var root = await document.GetSyntaxRootAsync();

            var newRoot = root.ReplaceNode(classDeclaration, newClassNode);
            var newDocument = document.WithSyntaxRoot(newRoot);

            return newDocument;
        }
    }
}