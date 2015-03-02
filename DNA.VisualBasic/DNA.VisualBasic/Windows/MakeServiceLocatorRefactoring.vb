Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CodeActions
Imports Microsoft.CodeAnalysis.CodeRefactorings
Imports Microsoft.CodeAnalysis.Rename
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

<ExportCodeRefactoringProvider(LanguageNames.VisualBasic, Name:=NameOf(MakeServiceLocatorRefactoring))>
Friend Class MakeServiceLocatorRefactoring
    Inherits CodeRefactoringProvider

    Public NotOverridable Overrides Async Function ComputeRefactoringsAsync(context As CodeRefactoringContext) As Task
        Dim root = Await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(False)

        ' Find the node at the selection.
        Dim node = root.FindNode(context.Span)

        ' Only offer a refactoring if the selected node is a class statement node.
        Dim classDecl = TryCast(node, ClassStatementSyntax)
        If classDecl Is Nothing Then Return

        Dim action = CodeAction.Create("Implement ServiceLocator", Function(c) MakeServiceLocatorAsync(context.Document, classDecl, c))

        ' Register this code action.
        context.RegisterRefactoring(action)
    End Function

    Private Async Function MakeServiceLocatorAsync(document As Document, classDeclaration As ClassStatementSyntax,
                             cancellationToken As CancellationToken) As Task(Of Document)

        Dim newClass = "Public Class ServiceLocator
    Implements IServiceProvider

    Private services As New Dictionary(Of Type, Object)()

    Public Function GetService(Of T)() As T
        Return CType(GetService(GetType(T)), T)
    End Function

    Public Function RegisterService(Of T)(ByVal service As T, ByVal overwriteIfExists As Boolean) As Boolean
        SyncLock services
            If Not services.ContainsKey(GetType(T)) Then
                services.Add(GetType(T), service)
                Return True
            ElseIf overwriteIfExists Then
                services(GetType(T)) = service
                Return True
            End If
        End SyncLock
        Return False
    End Function

    Public Function RegisterService(Of T)(ByVal service As T) As Boolean
        Return RegisterService(Of T)(service, True)
    End Function

    Public Function GetService(ByVal serviceType As Type) As Object Implements IServiceProvider.GetService
        SyncLock services
            If services.ContainsKey(serviceType) Then
                Return services(serviceType)
            End If
        End SyncLock
        Return Nothing
    End Function
End Class"

        Dim root = Await document.GetSyntaxRootAsync
        Dim newRoot As SyntaxNode = SyntaxFactory.ParseCompilationUnit(newClass)

        Dim newDocument = document.WithSyntaxRoot(newRoot)

        Return newDocument
    End Function
End Class