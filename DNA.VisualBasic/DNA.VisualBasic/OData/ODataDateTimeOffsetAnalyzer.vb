Imports System.Collections.Immutable
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class ODataDateTimeOffsetAnalyzer
    Inherits DiagnosticAnalyzer

    Public Const DiagnosticId = "DNA100"
    Friend Shared ReadOnly Title As String = "Declaration of type System.DateTime"
    Friend Shared ReadOnly MessageFormat As String = "Platform warning: '{0}'"
    Friend Shared ReadOnly Description As String = "Checks for improper usage of System.DateTime in OData"
    Friend Const Category = "Naming"

    Friend Shared Rule As New DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, True)

    Public Overrides ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor)
        Get
            Return ImmutableArray.Create(Rule)
        End Get
    End Property

    Public Overrides Sub Initialize(context As AnalysisContext)
        ' TODO: Consider registering other actions that act on syntax instead of or in addition to symbols
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeDate, SyntaxKind.PredefinedType)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeDate, SyntaxKind.IdentifierName)
    End Sub

    Private Sub AnalyzeDate(context As SyntaxNodeAnalysisContext)
        If context.SemanticModel.Compilation.GetTypeByMetadataName("Microsoft.OData.Core.ODataAction") Is Nothing Then
            Return
        End If

        Dim root = context.Node

        If TypeOf (root) Is PredefinedTypeSyntax Then
            root = CType(context.Node, PredefinedTypeSyntax)
        ElseIf TypeOf (root) Is IdentifierNameSyntax
            root = CType(context.Node, IdentifierNameSyntax)
        Else
            Return
        End If

        Dim dateSymbol = TryCast(context.SemanticModel.GetSymbolInfo(root).Symbol, INamedTypeSymbol)

        If dateSymbol Is Nothing Then
            Return
        End If

        If Not dateSymbol.MetadataName = "DateTime" Then
            Return
        End If

        Dim diagn = Diagnostic.Create(Rule, root.GetLocation,
                              "Consider replacing with DateTimeOffset")
        context.ReportDiagnostic(diagn)

    End Sub
End Class