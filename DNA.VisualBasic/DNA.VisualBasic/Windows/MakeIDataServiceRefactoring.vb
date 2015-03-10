Imports Microsoft.CodeAnalysis.CodeRefactorings
Imports Microsoft.CodeAnalysis.Formatting, Microsoft.CodeAnalysis.Simplification
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

    Private Async Function MakeIDataServiceAsync(document As Document, interfaceDeclaration As InterfaceStatementSyntax,
                            cancellationToken As CancellationToken) As Task(Of Document)

        Dim newImplementation = "Sub Add(ByVal dataSource As Object)
    Sub Delete(ByVal dataSource As Object)
    Sub Save(ByVal dataSource As Object)
    Function GetAllItems() As IQueryable(Of Object)
"


        Dim newInterfaceTree = SyntaxFactory.ParseSyntaxTree(newImplementation).
                GetRoot().DescendantNodes().
                Where(Function(n) n.IsKind(SyntaxKind.SubBlock) OrElse n.IsKind(SyntaxKind.FunctionBlock)).
                Cast(Of StatementSyntax).Select(Function(m) m.WithAdditionalAnnotations(Formatter.Annotation, Simplifier.Annotation)).ToArray

        Dim parentBlock = TryCast(interfaceDeclaration.Parent, InterfaceBlockSyntax)

        Dim newInterfaceBlock = SyntaxFactory.InterfaceBlock(SyntaxFactory.
            InterfaceStatement("IDataService").AddModifiers(SyntaxFactory.ParseToken("Public"))).
            WithTrailingTrivia(SyntaxFactory.EndOfLineTrivia(""))

        newInterfaceBlock = newInterfaceBlock.WithEndInterfaceStatement(SyntaxFactory.EndInterfaceStatement)

        Dim newClassNode = newInterfaceBlock.AddMembers(newInterfaceTree)

        Dim root = Await document.GetSyntaxRootAsync

        Dim newRoot As SyntaxNode = root.ReplaceNode(parentBlock, newClassNode)
        Dim newDocument = document.WithSyntaxRoot(newRoot)

        Return newDocument

    End Function
End Class