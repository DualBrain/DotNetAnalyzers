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

namespace DNA.CSharp.Windows
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(ODataFromODataUriCodeFix))]
    public class ODataFromODataUriCodeFix : CodeFixProvider
    {
        // TODO: Replace with actual diagnostic id that should trigger this fix.
        public const string DiagnosticId = "DNA102";

        public sealed override ImmutableArray<string> FixableDiagnosticIds
        {
            get { return ImmutableArray.Create(DiagnosticId); }
        }

        public sealed override FixAllProvider GetFixAllProvider()
        {
            return WellKnownFixAllProviders.BatchFixer;
        }

        public async sealed override Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            // TODO: Replace the following code with your own analysis, generating a CodeAction for each fix to suggest

            var diagnostic = context.Diagnostics.First();
            var diagnosticSpan = diagnostic.Location.SourceSpan;

            var node = root.FindNode(context.Span) as ParameterSyntax;

            context.RegisterCodeFix(
            CodeAction.Create("Add [FromODataUri]",
                              c => ApplyFromODataUriAsync(context.Document, node, c)), diagnostic);
        }

        private async Task<Document> ApplyFromODataUriAsync(Document document, ParameterSyntax node,
                         CancellationToken cancellationToken)
        {
            var root = await document.GetSyntaxRootAsync();

            var newParameter = node.WithAttributeLists(node.AttributeLists.Add(SyntaxFactory.AttributeList(
                        SyntaxFactory.SingletonSeparatedList<AttributeSyntax>(
                            SyntaxFactory.Attribute(
                                SyntaxFactory.IdentifierName("FromODataUri"))))));

            var newRoot = root.ReplaceNode(node, newParameter);
            var newDocument = document.WithSyntaxRoot(newRoot);
            return newDocument;

        }
    }
}