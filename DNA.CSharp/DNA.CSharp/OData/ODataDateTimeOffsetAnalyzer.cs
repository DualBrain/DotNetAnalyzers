using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace DNA.CSharp.OData
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class ODataDateTimeOffsetAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "DNA100";
        internal static readonly string Title = "Declaration of type System.DateTime";
        internal static readonly string MessageFormat = "Platform warning: '{0}'";
        internal const string Category = "Naming";

        internal static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, true);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(AnalyzeDate, SyntaxKind.PredefinedType);
            context.RegisterSyntaxNodeAction(AnalyzeDate, SyntaxKind.IdentifierName);
        }

        private void AnalyzeDate(SyntaxNodeAnalysisContext context)
        {
            if (context.SemanticModel.Compilation.GetTypeByMetadataName("Microsoft.OData.Core.ODataAction") == null)
            {
                return;
            }

            var root = context.Node;

            if ((root) is PredefinedTypeSyntax)
            {
                root = (PredefinedTypeSyntax)context.Node;
            }
            else if ((root) is IdentifierNameSyntax)
            {
                root = (IdentifierNameSyntax)context.Node;
            }
            else
            {
                return;
            }

            var dateSymbol = context.SemanticModel.GetSymbolInfo(root).Symbol as INamedTypeSymbol;

            if (dateSymbol == null)
            {
                return;
            }

            if (!(dateSymbol.MetadataName == "DateTime"))
            {
                return;
            }

            var diagn = Diagnostic.Create(Rule, root.GetLocation(), "Consider replacing with DateTimeOffset");
            context.ReportDiagnostic(diagn);

        }

    }
}