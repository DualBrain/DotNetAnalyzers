Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CodeActions
Imports Microsoft.CodeAnalysis.CodeRefactorings
Imports Microsoft.CodeAnalysis.Formatting
Imports Microsoft.CodeAnalysis.Rename
Imports Microsoft.CodeAnalysis.Simplification
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Extensions
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

<ExportCodeRefactoringProvider(LanguageNames.VisualBasic, Name:=NameOf(MakeRelayCommandRefactoring))>
Friend Class MakeRelayCommandRefactoring
    Inherits CodeRefactoringProvider

    Public NotOverridable Overrides Async Function ComputeRefactoringsAsync(context As CodeRefactoringContext) As Task

        Dim root = Await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(False)

        ' Find the node at the selection.
        Dim node = root.FindNode(context.Span)

        ' Only offer a refactoring if the selected node is a class statement node.
        Dim classDecl = TryCast(node, ClassStatementSyntax)
        If classDecl Is Nothing Then Return

        Dim action = CodeAction.Create("Implement RelayCommand(Of T)", Function(c) RebuildClassAsync(context.Document, classDecl, c))

        ' Register this code action.
        context.RegisterRefactoring(action)
    End Function

    Private Async Function RebuildClassAsync(document As Document, classDeclaration As ClassStatementSyntax,
                                        cancellationToken As CancellationToken) As Task(Of Document)

        Dim newImplementation = "Implements ICommand

    Private ReadOnly _execute As Action(Of T)
    Private ReadOnly _canExecute As Predicate(Of T)

    Public Sub New(ByVal execute As Action(Of T))
        Me.New(execute, Nothing)
    End Sub

    Public Sub New(ByVal execute As Action(Of T), ByVal canExecute As Predicate(Of T))
        If execute Is Nothing Then
            Throw New ArgumentNullException(""execute"")
        End If
        _execute = execute
        _canExecute = canExecute
    End Sub

    <DebuggerStepThrough()> _
    Public Function CanExecute(ByVal parameter As Object) As Boolean Implements ICommand.CanExecute
        Return If(_canExecute Is Nothing, True, _canExecute(CType(parameter, T)))
    End Function

    Public Custom Event CanExecuteChanged As EventHandler Implements ICommand.CanExecuteChanged
        AddHandler(ByVal value As EventHandler)
            AddHandler CommandManager.RequerySuggested, value
        End AddHandler
        RemoveHandler(ByVal value As EventHandler)
            RemoveHandler CommandManager.RequerySuggested, value
        End RemoveHandler
        RaiseEvent(ByVal sender As System.Object, ByVal e As System.EventArgs)
        End RaiseEvent
    End Event

    Public Sub Execute(ByVal parameter As Object) Implements ICommand.Execute
        _execute(CType(parameter, T))
    End Sub"

        Dim newClassTree = SyntaxFactory.ParseSyntaxTree(newImplementation).
                GetRoot().DescendantNodes().
                Where(Function(n) n.IsKind(SyntaxKind.FieldDeclaration) OrElse n.IsKind(SyntaxKind.SubBlock) OrElse n.IsKind(SyntaxKind.FunctionBlock) _
                OrElse n.IsKind(SyntaxKind.ConstructorBlock) OrElse n.IsKind(SyntaxKind.EventBlock)).
                Cast(Of StatementSyntax).
                Select(Function(decl) decl.WithAdditionalAnnotations(Formatter.Annotation, Simplifier.Annotation)).
                ToArray()

        Dim parentBlock = TryCast(classDeclaration.Parent, ClassBlockSyntax)

        Dim newClassBlock = SyntaxFactory.ClassBlock(SyntaxFactory.ClassStatement("RelayCommand").
                                                     AddTypeParameterListParameters(SyntaxFactory.TypeParameter("T")))

        newClassBlock = newClassBlock.AddImplements(SyntaxFactory.
                                                    ImplementsStatement(SyntaxFactory.ParseToken("Implements"),
                                                                        SyntaxFactory.SingletonSeparatedList(Of TypeSyntax) _
                                                                        (SyntaxFactory.ParseTypeName("ICommand"))))
        newClassBlock = newClassBlock.WithEndClassStatement(SyntaxFactory.EndClassStatement())

        Dim newClassNode = newClassBlock.AddMembers(newClassTree)

        Dim root = Await document.GetSyntaxRootAsync

        Dim newRoot As SyntaxNode = root.ReplaceNode(parentBlock, newClassNode)
        Dim newDocument = document.WithSyntaxRoot(newRoot)

        Return newDocument
    End Function
End Class