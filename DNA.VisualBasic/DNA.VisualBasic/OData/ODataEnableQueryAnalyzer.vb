Imports System.Collections.Immutable
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class ODataEnableQueryAnalyzer
    Inherits DiagnosticAnalyzer

    Public Const DiagnosticId = "DNA101"
    Friend Shared ReadOnly Title As String = "OData warning"
    Friend Shared ReadOnly MessageFormat As String = "'{0}' method should be decorated with EnableQuery"
    Friend Const Category = "Syntax"

    Friend Shared Rule As New DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, True)

    Public Overrides ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor)
        Get
            Return ImmutableArray.Create(Rule)
        End Get
    End Property

    Public Overrides Sub Initialize(context As AnalysisContext)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeEnableQuery, SyntaxKind.FunctionStatement
                                         )
    End Sub

    Private Sub AnalyzeEnableQuery(context As SyntaxNodeAnalysisContext)
        If context.SemanticModel.Compilation.GetTypeByMetadataName("Microsoft.OData.Core.ODataAction") Is Nothing Then
            Return
        End If

        Dim root = context.Node
        Dim method As MethodStatementSyntax
        If TypeOf (root) Is MethodStatementSyntax Then
            method = TryCast(context.Node, MethodStatementSyntax)
        Else
            Return
        End If

        If method.Identifier.Value.ToString.ToLowerInvariant.StartsWith("[get") Or method.Identifier.Value.ToString.ToLowerInvariant.StartsWith("get") Then
            Dim descendantAttribs = method.AttributeLists.Where(Function(a) a.ToString.Contains("<EnableQuery>")).FirstOrDefault
            If descendantAttribs Is Nothing Then
                Dim diagn = Diagnostic.Create(Rule, root.GetLocation,
                      method.Identifier.Value.ToString)
                context.ReportDiagnostic(diagn)
            Else
                Return
            End If
        Else
            Return
        End If
    End Sub


End Class