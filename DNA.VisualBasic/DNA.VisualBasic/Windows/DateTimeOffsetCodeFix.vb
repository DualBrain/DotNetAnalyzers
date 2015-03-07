Imports System.Collections.Immutable
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CodeActions
Imports Microsoft.CodeAnalysis.CodeFixes
Imports Microsoft.CodeAnalysis.CodeRefactorings
Imports Microsoft.CodeAnalysis.Editing
Imports Microsoft.CodeAnalysis.Rename
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

<ExportCodeFixProvider(LanguageNames.VisualBasic, Name:=NameOf(DateTimeOffsetCodeFix))>
Public Class DateTimeOffsetCodeFix
    Inherits CodeFixProvider

    ' TODO: Replace with actual diagnostic id that should trigger this fix.
    Public Const DiagnosticId As String = DateTimeOffsetAnalyzer.DiagnosticId

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

        Dim node = root.FindNode(context.Span)

        If TypeOf (node) Is PredefinedTypeSyntax Then
            context.RegisterCodeFix(
            CodeAction.Create("Replace with DateTimeOffset",
                              Function(c) ReplaceDateAsync(context.Document, node, c)), diagnostic)
        Else
            context.RegisterCodeFix(
            CodeAction.Create("Replace with DateTimeOffset",
                              Function(c) ReplaceDateTimeAsync(context.Document, node, c)), diagnostic)
        End If
    End Function

    Private Async Function ReplaceDateAsync(document As Document, node As PredefinedTypeSyntax,
                                     cancellationToken As CancellationToken) As Task(Of Document)

        'Dim semanticModel = Await document.GetSemanticModelAsync(cancellationToken)
        Dim root = Await document.GetSyntaxRootAsync
        Dim generator = SyntaxGenerator.GetGenerator(document)

        Dim newIdentifierSyntax = generator.IdentifierName("DateTimeOffset").WithTrailingTrivia(node.GetTrailingTrivia)

        Dim newRoot = root.ReplaceNode(node, newIdentifierSyntax)
        Dim newDocument = document.WithSyntaxRoot(newRoot)
        Return newDocument
    End Function

    Private Async Function ReplaceDateTimeAsync(document As Document, node As IdentifierNameSyntax,
                                     cancellationToken As CancellationToken) As Task(Of Document)

        Dim semanticModel = Await document.GetSemanticModelAsync(cancellationToken)
        Dim root = Await document.GetSyntaxRootAsync

        'DateTime
        Dim node2 = node.WithIdentifier(SyntaxFactory.ParseToken("DateTimeOffset")).WithTrailingTrivia(node.GetTrailingTrivia)
        Dim newRoot = root.ReplaceNode(node, node2)

        Dim newDocument = document.WithSyntaxRoot(newRoot)
        Return newDocument

    End Function
End Class