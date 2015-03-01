Module HelperMethods
    Async Function IsWPF(context As CodeRefactorings.CodeRefactoringContext) As Task(Of Boolean)
        Dim model = Await context.Document.GetSemanticModelAsync
        Dim isSupported = model.Compilation.GetTypeByMetadataName("System.Windows.Navigation.JournalEntry")

        If isSupported Is Nothing Then
            Return False
        Else
            Return True
        End If
    End Function
End Module
