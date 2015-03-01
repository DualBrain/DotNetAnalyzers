Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CodeActions
Imports Microsoft.CodeAnalysis.CodeRefactorings
Imports Microsoft.CodeAnalysis.Rename
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

<ExportCodeRefactoringProvider(LanguageNames.VisualBasic, Name:=NameOf(MakeViewModelBaseRefactoring))>
Friend Class MakeViewModelBaseRefactoring
    Inherits CodeRefactoringProvider

    Public NotOverridable Overrides Async Function ComputeRefactoringsAsync(context As CodeRefactoringContext) As Task
        Dim root = Await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(False)

        ' Find the node at the selection.
        Dim node = root.FindNode(context.Span)

        ' Only offer a refactoring if the selected node is a class statement node.
        Dim classDecl = TryCast(node, ClassStatementSyntax)
        If classDecl Is Nothing Then Return

        Dim action = CodeAction.Create("Implement ViewModelBase", Function(c) MakeViewModelBaseAsync(context.Document, classDecl, c))

        ' Register this code action.
        context.RegisterRefactoring(action)
    End Function

    Private Async Function MakeViewModelBaseAsync(document As Document, classDeclaration As ClassStatementSyntax,
                                     cancellationToken As CancellationToken) As Task(Of Document)

        Dim newClass = "Imports System.ComponentModel

Public MustInherit Class ViewModelBase
    Implements INotifyPropertyChanged

    Public Event PropertyChanged(ByVal sender As Object, ByVal e As System.ComponentModel.PropertyChangedEventArgs) Implements System.ComponentModel.INotifyPropertyChanged.PropertyChanged

    Protected Sub OnPropertyChanged(ByVal strPropertyName As String)

        If Me.PropertyChangedEvent IsNot Nothing Then
            RaiseEvent PropertyChanged(Me, New System.ComponentModel.PropertyChangedEventArgs(strPropertyName))
        End If

    End Sub

    Private privateThrowOnInvalidPropertyName As Boolean
    Protected Overridable Property ThrowOnInvalidPropertyName() As Boolean
        Get
            Return privateThrowOnInvalidPropertyName
        End Get
        Set(ByVal value As Boolean)
            privateThrowOnInvalidPropertyName = value
        End Set
    End Property

    <Conditional(""DEBUG""), DebuggerStepThrough()> _
    Public Sub VerifyPropertyName(ByVal propertyName As String)
        ' Verify that the property name matches a real,  
        ' public, instance property on this object.
        If TypeDescriptor.GetProperties(Me)(propertyName) Is Nothing Then
            Dim msg As String = ""Invalid Property name: "" & propertyName

            If Me.ThrowOnInvalidPropertyName Then
                Throw New Exception(msg)
            Else
                Debug.Fail(msg)
            End If
        End If
    End Sub

    Private privateDisplayName As String
    Public Overridable Property DisplayName() As String
        Get
            Return privateDisplayName
        End Get
        Protected Set(ByVal value As String)
            privateDisplayName = value
        End Set
    End Property

    'Uncomment the following lines if you implement a service locator
    'Dim myServiceLocator As New ServiceLocator

    'Public Function ServiceLocator() As ServiceLocator
    '    Return Me.myServiceLocator
    'End Function

    'Public Function GetService(Of T)() As T
    '    Return myServiceLocator.GetService(Of T)()
    'End Function
End Class"

        Dim root = Await document.GetSyntaxRootAsync
        Dim newRoot As SyntaxNode = SyntaxFactory.ParseCompilationUnit(newClass)

        Dim newDocument = document.WithSyntaxRoot(newRoot)

        Return newDocument
    End Function
End Class
