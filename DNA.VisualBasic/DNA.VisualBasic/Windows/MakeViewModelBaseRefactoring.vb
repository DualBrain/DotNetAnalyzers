Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CodeActions
Imports Microsoft.CodeAnalysis.CodeRefactorings, Microsoft.CodeAnalysis.Formatting, Microsoft.CodeAnalysis.Simplification
Imports Microsoft.CodeAnalysis.Editing
Imports Microsoft.CodeAnalysis.Rename
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Extensions
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports System.Composition
Imports Microsoft.CodeAnalysis.Host.Mef

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

        Dim newImplementation = $"
    '{My.Resources.LocalResources.Uncomment}
    'Dim myServiceLocator As New ServiceLocator

    'Public Function ServiceLocator() As ServiceLocator
    '    Return Me.myServiceLocator
    'End Function

    'Public Function GetService(Of T)() As T
    '    Return myServiceLocator.GetService(Of T)()
    'End Function
    
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
        If System.ComponentModel.TypeDescriptor.GetProperties(Me)(propertyName) Is Nothing Then
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
"

        Dim newClassRoot = SyntaxFactory.ParseSyntaxTree(newImplementation).GetRoot()
        Dim newClassTree = newClassRoot.DescendantNodes().
                Where(Function(n) n.IsKind(SyntaxKind.FieldDeclaration) OrElse n.IsKind(SyntaxKind.SubBlock) OrElse n.IsKind(SyntaxKind.FunctionBlock) _
                OrElse n.IsKind(SyntaxKind.ConstructorBlock) OrElse n.IsKind(SyntaxKind.EventStatement) OrElse
                n.IsKind(SyntaxKind.PropertyBlock)).
                Cast(Of StatementSyntax).
                Select(Function(decl) decl.WithAdditionalAnnotations(Formatter.Annotation, Simplifier.Annotation)).
                ToArray()

        Dim parentBlock = TryCast(classDeclaration.Parent, ClassBlockSyntax)

        Dim generator = SyntaxGenerator.GetGenerator(document)

        Dim newClassBlock = SyntaxFactory.ClassBlock(SyntaxFactory.ClassStatement("ViewModelBase").
                                                     AddModifiers(SyntaxFactory.ParseToken("MustInherit")))

        newClassBlock = generator.WithAccessibility(newClassBlock, Accessibility.Public)

        newClassBlock = newClassBlock.AddImplements(SyntaxFactory.
                                                    ImplementsStatement(SyntaxFactory.ParseToken("Implements"),
                                                                        SyntaxFactory.SingletonSeparatedList(Of TypeSyntax) _
                                                                        (SyntaxFactory.ParseTypeName("System.ComponentModel.INotifyPropertyChanged"))))

        newClassBlock = newClassBlock.WithEndClassStatement(SyntaxFactory.EndClassStatement())

        Dim newClassNode = newClassBlock.AddMembers(newClassTree)

        Dim root = Await document.GetSyntaxRootAsync

        Dim newRoot As SyntaxNode = root.ReplaceNode(parentBlock, newClassNode).NormalizeWhitespace

        Dim newDocument = document.WithSyntaxRoot(newRoot)

        Return newDocument
    End Function
End Class

