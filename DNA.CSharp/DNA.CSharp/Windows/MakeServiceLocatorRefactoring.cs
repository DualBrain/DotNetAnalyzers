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
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(MakeServiceLocatorRefactoring))]
    internal class MakeServiceLocatorRefactoring : CodeRefactoringProvider
    {
        public async sealed override Task ComputeRefactoringsAsync(CodeRefactoringContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            var node = root.FindNode(context.Span);

            var classDecl = node as ClassDeclarationSyntax;

                    if (classDecl ==null) { return; }

            var action = CodeAction.Create("Implement ServiceLocator", c => MakeServiceLocatorAsync(context.Document, classDecl, c));

            // Register this code action.
            context.RegisterRefactoring(action);
        }

        private async Task<Document> MakeServiceLocatorAsync(Document document, ClassDeclarationSyntax classDeclaration, CancellationToken cancellationToken)
        {

            var newImplementation = @"
private Dictionary<Type, object> services = new Dictionary<Type, object>();
#pragma warning disable CS0693
public T GetService<T>()
{
	return (T)GetService(typeof(T));
}

public bool RegisterService<T>(T service, bool overwriteIfExists)
{
	lock (services) {
		if (!services.ContainsKey(typeof(T))) {
			services.Add(typeof(T), service);
			return true;
		} else if (overwriteIfExists) {
			services[typeof(T)] = service;
			return true;
		}
	}
	return false;
}

public bool RegisterService<T>(T service)
{
	return RegisterService<T>(service, true);
}

public object GetService(Type serviceType)
{
	lock (services) {
		if (services.ContainsKey(serviceType)) {
			return services[serviceType];
		}
	}
	return null;
}
";

            var newClassTree = SyntaxFactory.ParseSyntaxTree(newImplementation).
        GetRoot().DescendantNodes().
        Where(n => n.IsKind(SyntaxKind.FieldDeclaration) || n.IsKind(SyntaxKind.MethodDeclaration) || n.IsKind(SyntaxKind.PropertyDeclaration)
                || n.IsKind(SyntaxKind.ConstructorDeclaration) || n.IsKind(SyntaxKind.EventDeclaration) || n.IsKind(SyntaxKind.EventFieldDeclaration)).
        Cast<MemberDeclarationSyntax>().
        Select(decl => decl.WithAdditionalAnnotations(Formatter.Annotation, Simplifier.Annotation)).
        ToArray();


            ClassDeclarationSyntax newClassBlock = SyntaxFactory.ClassDeclaration("ServiceLocator").AddTypeParameterListParameters(SyntaxFactory.TypeParameter("T")).WithOpenBraceToken(SyntaxFactory.ParseToken("{")).
                    WithCloseBraceToken(SyntaxFactory.ParseToken("}").WithTrailingTrivia(SyntaxFactory.CarriageReturnLineFeed));

            var newClassNode = newClassBlock.AddMembers(newClassTree);

            var root = await document.GetSyntaxRootAsync();

            var newRoot = root.ReplaceNode(classDeclaration, newClassNode);
            var newDocument = document.WithSyntaxRoot(newRoot);

            return newDocument;
        }

    }
}