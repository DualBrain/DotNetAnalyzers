Imports System.Collections.Immutable
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CodeActions
Imports Microsoft.CodeAnalysis.CodeFixes
Imports Microsoft.CodeAnalysis.CodeRefactorings
Imports Microsoft.CodeAnalysis.Rename
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

<ExportCodeFixProvider(LanguageNames.VisualBasic, Name:=NameOf(ODataCheckFromODataUriCodeFix))>
Public Class ODataCheckFromODataUriCodeFix
    Inherits CodeFixProvider

    ' TODO: Replace with actual diagnostic id that should trigger this fix.
    Public Const DiagnosticId As String = "DNA102"

    Public NotOverridable Overrides ReadOnly Property FixableDiagnosticIds As ImmutableArray(Of String)
        Get
            Return ImmutableArray.Create(DiagnosticId)
        End Get
    End Property

    Public NotOverridable Overrides Function GetFixAllProvider() As FixAllProvider
        Return WellKnownFixAllProviders.BatchFixer
    End Function

    Public NotOverridable Overrides Async Function RegisterCodeFixesAsync(context As CodeFixContext) As Task
        Dim root = Await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(False)

        ' TODO: Replace the following code with your own analysis, generating a CodeAction for each fix to suggest

        Dim diagnostic = context.Diagnostics.First()
        Dim diagnosticSpan = diagnostic.Location.SourceSpan

        Dim node = TryCast(root.FindNode(context.Span), ParameterSyntax)

        context.RegisterCodeFix(
        CodeAction.Create("Add <FromODataUri>",
                          Function(c) ApplyFromODataUriAsync(context.Document, node, c)), diagnostic)
    End Function

    Private Async Function ApplyFromODataUriAsync(document As Document, node As ParameterSyntax,
                             cancellationToken As CancellationToken) As Task(Of Document)

        Dim root = Await document.GetSyntaxRootAsync

        Dim newParameter = node.WithAttributeLists(node.AttributeLists.Add(SyntaxFactory.AttributeList(
                    SyntaxFactory.SingletonSeparatedList(Of AttributeSyntax)(
                        SyntaxFactory.Attribute(
                            SyntaxFactory.IdentifierName("FromODataUri"))))))

        Dim newRoot = root.ReplaceNode(node, newParameter)
        Dim newDocument = document.WithSyntaxRoot(newRoot)
        Return newDocument
    End Function
End Class