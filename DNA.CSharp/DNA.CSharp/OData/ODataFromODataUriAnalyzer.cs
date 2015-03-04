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
    public class ODataFromODataUriAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "DNA102";
        internal static readonly string Title = "OData warning";
        internal static readonly string MessageFormat = "'{0}' is the first parameter and should be decorated with [FromODataUri]";
        internal const string Category = "Syntax";

        internal static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, true);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(AnalyzeFromODataUri, SyntaxKind.MethodDeclaration);
        }


        private void AnalyzeFromODataUri(SyntaxNodeAnalysisContext context)
        {
            if (context.SemanticModel.Compilation.GetTypeByMetadataName("Microsoft.OData.Core.ODataAction") == null)
            {
                return;
            }

            var root = context.Node;
            MethodDeclarationSyntax method;
            if (root is MethodDeclarationSyntax)
            {
                method = context.Node as MethodDeclarationSyntax;
            }
            else
            {
                return;
            }

            List<string> supportedMethods = new List<string> {
        "Get",
        "Post",
        "Put",
        "Delete",
        "Patch",
        "[Get]"
    };


            if (supportedMethods.Contains(method.Identifier.Value.ToString()))
            {
                //Has parameters?
                var firstParameterList = root.DescendantNodes().OfType<ParameterListSyntax>().FirstOrDefault();
                if (firstParameterList == null)
                {
                    return;
                }

                var firstParameter = firstParameterList.Parameters.FirstOrDefault();
                if (firstParameter == null)
                {
                    return;
                }
                
                if (firstParameter.AttributeLists.Count == 0)
                {
                    //No attributes, so must add one
                    var diagn = Diagnostic.Create(Rule, firstParameter.GetLocation(), method.ParameterList.Parameters.FirstOrDefault().Identifier.ToString());
                    context.ReportDiagnostic(diagn);
                }
                else
                {
                    //For each attrib
                    foreach (var attrib in firstParameter.AttributeLists)
                    {
                        //If at least one contains FromODataUri, return
                        if (attrib.Attributes.Any(a => a.Name.ToString().ToLowerInvariant().Contains("fromodatauri")))
                        {
                            return;
                        }
                    }
                    //If not, create a diagnostic
                    var diagn = Diagnostic.Create(Rule, firstParameter.GetLocation(), method.ParameterList.Parameters.FirstOrDefault().Identifier.ToString());
                    context.ReportDiagnostic(diagn);
                }

            }
            else
            {
                return;
            }
        }
    }
}