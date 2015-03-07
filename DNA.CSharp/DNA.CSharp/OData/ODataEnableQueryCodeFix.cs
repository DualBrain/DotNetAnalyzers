using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;

namespace DNA.CSharp.OData
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(ODataEnableQueryCodeFix))]
    public class ODataEnableQueryCodeFix : CodeFixProvider
    {
        // TODO: Replace with actual diagnostic id that should trigger this fix.
        public const string DiagnosticId = "DNA101";

        public sealed override ImmutableArray<string> FixableDiagnosticIds
        {
            get { return ImmutableArray.Create(DiagnosticId); }
        }

        public sealed override FixAllProvider GetFixAllProvider()
        {
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed async override Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            var diagnostic = context.Diagnostics.First();
            var diagnosticSpan = diagnostic.Location.SourceSpan;

            var node = root.FindNode(context.Span) as MethodDeclarationSyntax;

            context.RegisterCodeFix(
            CodeAction.Create("Add [EnableQuery]",
                              c => EnableQueryAsync(context.Document, node, c)), diagnostic);
        }

        private async Task<Document> EnableQueryAsync(Document document, MethodDeclarationSyntax node,
                             CancellationToken cancellationToken)
        {
            var root = await document.GetSyntaxRootAsync();
            var newMethod = node.WithoutLeadingTrivia().WithAttributeLists(node.AttributeLists.Add(SyntaxFactory.AttributeList(
            SyntaxFactory.SingletonSeparatedList<AttributeSyntax>(
                SyntaxFactory.Attribute(
                    SyntaxFactory.IdentifierName("EnableQuery")))))).WithLeadingTrivia(node.GetLeadingTrivia());
            var newRoot = root.ReplaceNode(node, newMethod);
            var newDocument = document.WithSyntaxRoot(newRoot);
            return newDocument;
        }

    }
}