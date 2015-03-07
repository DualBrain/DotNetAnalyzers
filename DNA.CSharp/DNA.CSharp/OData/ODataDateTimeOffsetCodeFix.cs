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
using Microsoft.CodeAnalysis.Editing;

namespace DNA.CSharp.OData
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(ODataDateTimeOffsetCodeFix))]
    public class ODataDateTimeOffsetCodeFix : CodeFixProvider
    {
        // TODO: Replace with actual diagnostic id that should trigger this fix.
        public const string DiagnosticId = "DNA100";

        public sealed override ImmutableArray<string> FixableDiagnosticIds
        {
            get { return ImmutableArray.Create(DiagnosticId); }
        }

        public sealed override FixAllProvider GetFixAllProvider()
        {
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            throw new NotImplementedException();
        }

        private async Task<Document> ReplaceDateAsync(Document document, PredefinedTypeSyntax node,
                                 CancellationToken cancellationToken)
        {
            var root = await document.GetSyntaxRootAsync();
            var generator = SyntaxGenerator.GetGenerator(document);

            var newIdentifierSyntax = generator.IdentifierName("DateTimeOffset").WithTrailingTrivia(node.GetTrailingTrivia());

            var newRoot = root.ReplaceNode(node, newIdentifierSyntax);
            var newDocument = document.WithSyntaxRoot(newRoot);
            return newDocument;

        }

        private async Task<Document> ReplaceDateTimeAsync(Document document, PredefinedTypeSyntax node,
                         CancellationToken cancellationToken)
        {
            var semanticModel = await document.GetSemanticModelAsync(cancellationToken);
        var root = await document.GetSyntaxRootAsync();

            var node2 = node.WithKeyword(SyntaxFactory.ParseToken("DateTimeOffset")).WithTrailingTrivia(node.GetTrailingTrivia());
            var newRoot = root.ReplaceNode(node, node2);

            var newDocument = document.WithSyntaxRoot(newRoot);
            return newDocument;
        }
    }
}