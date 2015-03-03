Imports System.Collections.Immutable
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class ODataCheckFromODataUriAnalyzer
    Inherits DiagnosticAnalyzer

    Public Const DiagnosticId = "DNA102"
    Friend Shared ReadOnly Title As String = "OData warning"
    Friend Shared ReadOnly MessageFormat As String = "'{0}' is the first parameter and should be decorated with <FromODataUri>"
    Friend Const Category = "Syntax"

    Friend Shared Rule As New DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, True)

    Public Overrides ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor)
        Get
            Return ImmutableArray.Create(Rule)
        End Get
    End Property

    Public Overrides Sub Initialize(context As AnalysisContext)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeFromODataUri, SyntaxKind.FunctionStatement
                                         )
    End Sub

    Private Sub AnalyzeFromODataUri(context As SyntaxNodeAnalysisContext)
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

        Dim supportedMethods As New List(Of String) From {"Get", "Post", "Put", "Delete", "Patch", "[Get]"}

        If supportedMethods.Contains(method.Identifier.Value.ToString) Then

            'Has parameters?
            Dim firstParameterList = root.DescendantNodes.OfType(Of ParameterListSyntax).FirstOrDefault
            If firstParameterList Is Nothing Then
                Return
            End If

            Dim firstParameter = firstParameterList.Parameters.FirstOrDefault
            If firstParameter Is Nothing Then
                Return
            End If

            If firstParameter.AttributeLists = Nothing Then
                'No attributes, so must add one
                Dim diagn = Diagnostic.Create(Rule, firstParameter.GetLocation,
                method.ParameterList.Parameters.FirstOrDefault.Identifier.ToString)
                context.ReportDiagnostic(diagn)
            Else
                'For each attrib
                For Each attrib In firstParameter.AttributeLists
                    'If at least one contains FromODataUri, return
                    If attrib.Attributes.Any(Function(a) a.Name.ToString.ToLowerInvariant.Contains("fromodatauri")) Then
                        Return
                    End If
                Next
                'If not, create a diagnostic
                Dim diagn = Diagnostic.Create(Rule, firstParameter.GetLocation,
                    method.ParameterList.Parameters.FirstOrDefault.Identifier.ToString)
                context.ReportDiagnostic(diagn)
            End If

        Else
            Return
        End If
    End Sub


End Class