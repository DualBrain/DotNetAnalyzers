Imports Microsoft.CodeAnalysis.CodeRefactorings
Imports Microsoft.CodeAnalysis.Editing

<ExportCodeRefactoringProvider(LanguageNames.VisualBasic, Name:=NameOf(MakeIDataServiceRefactoring))>
Friend Class MakeIDataServiceRefactoring
    Inherits CodeRefactoringProvider

    Public NotOverridable Overrides Async Function ComputeRefactoringsAsync(context As CodeRefactoringContext) As Task
        Dim wpf = Await IsWPF(context)
        If wpf = False Then Return

        Dim root = Await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(False)

        ' Find the node at the selection.
        Dim node = root.FindNode(context.Span)

        ' Only offer a refactoring if the selected node is a class statement node.
        Dim classDecl = TryCast(node, InterfaceStatementSyntax)
        If classDecl Is Nothing Then Return

        Dim action = CodeAction.Create("Implement IDataService", Function(c) MakeIDataServiceAsync(context.Document, classDecl, c))

        ' Register this code action.
        context.RegisterRefactoring(action)
    End Function

    Private Async Function MakeIDataServiceAsync(document As Document, classDeclaration As InterfaceStatementSyntax,
                            cancellationToken As CancellationToken) As Task(Of Document)

        Dim newClass = "Public Interface IDataService

    Sub Add(ByVal dataSource As Object)
    Sub Delete(ByVal dataSource As Object)
    Sub Save(ByVal dataSource As Object)
    Function GetAllItems() As IQueryable(Of Object)
End Interface"

        Dim root = Await document.GetSyntaxRootAsync
        'Dim a = root.DescendantNodes.OfType(Of InterfaceStatementSyntax).Where(Function(i) i.Identifier.ToString = classDeclaration.Identifier.ToString).FirstOrDefault

        Dim newRoot As SyntaxNode = SyntaxFactory.ParseCompilationUnit(newClass)

        Dim newDocument = document.WithSyntaxRoot(newRoot)

        Return newDocument
    End Function
End Class