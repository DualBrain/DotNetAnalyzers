using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeRefactorings;

namespace DNA.CSharp.Helpers
{
    static class HelperMethods
    {
        async static Task<bool> IsWPF(CodeRefactoringContext context)
        {
            var model = await context.Document.GetSemanticModelAsync();
            var isSupported = model.Compilation.GetTypeByMetadataName("System.Windows.Navigation.JournalEntry");

            if (isSupported==null)
            {
                return false;
            }
            else
            {
                return true;
            }
        }
    }
}
