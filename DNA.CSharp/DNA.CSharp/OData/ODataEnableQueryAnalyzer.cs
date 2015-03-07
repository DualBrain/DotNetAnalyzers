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
    public class ODataEnableQueryAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "DNA101";
        internal static readonly string Title = "OData warning";
        internal static readonly string MessageFormat = "'{0}' method should be decorated with EnableQuery";
        internal const string Category = "Syntax";

        internal static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, true);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(AnalyzeEnableQuery, SyntaxKind.MethodDeclaration);
        }


        private void AnalyzeEnableQuery(SyntaxNodeAnalysisContext context)
        {
            if (context.SemanticModel.Compilation.GetTypeByMetadataName("Microsoft.OData.Core.ODataAction") == null)
            {
                return;
            }

            var root = context.Node;
            MethodDeclarationSyntax method;
            if ((root) is MethodDeclarationSyntax)
            {
                method = context.Node as MethodDeclarationSyntax;
            }
            else
            {
                return;
            }

            if (method.Identifier.Value.ToString().ToLowerInvariant().StartsWith("get"))
            {
                var descendantAttribs = method.AttributeLists.Where(a => a.ToString().Contains("[EnableQuery]")).FirstOrDefault();
                if (descendantAttribs == null)
                {
                    var diagn = Diagnostic.Create(Rule, root.GetLocation(), method.Identifier.Value.ToString());
                    context.ReportDiagnostic(diagn);
                }
                else
                {
                    return;
                }
            }
            else
            {
                return;
            }
        }
    }
}