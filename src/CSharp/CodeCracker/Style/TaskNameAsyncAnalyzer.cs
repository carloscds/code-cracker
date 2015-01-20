using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using System.Collections.Immutable;
using System.Threading.Tasks;

namespace CodeCracker.Style
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class TaskNameAsyncAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "CC0061";
        internal const string Title = "Async method can be terminating with 'Async' name.";
        internal const string MessageFormat = "Change method name to {0}";
        internal const string Category = SupportedCategories.Style;
        const string Description = "Async method can be terminating with 'Async' name.";

        internal static DiagnosticDescriptor Rule = new DiagnosticDescriptor(
            DiagnosticId,
            Title,
            MessageFormat,
            Category,
            DiagnosticSeverity.Info,
            isEnabledByDefault: true,
            description: Description,
            helpLink: HelpLink.ForDiagnostic(DiagnosticId));

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(AnalyzeMethod, SyntaxKind.MethodDeclaration);
        }

        private static void AnalyzeMethod(SyntaxNodeAnalysisContext context)
        {
            var method = (MethodDeclarationSyntax)context.Node;
            if (method.Identifier.ToString().EndsWith("Async")) return;

            var returnType = context.SemanticModel.GetSymbolInfo(method.ReturnType).Symbol as INamedTypeSymbol;
            if (returnType == null) return;
            returnType.ToString().StartsWith("System.Threading.Tasks.Task");

            if (method.Modifiers.Any(SyntaxKind.AsyncKeyword))
            {
                ReportDiagnostic(context, method);
                return;
            }
            if (method.Modifiers.Any(SyntaxKind.AsyncKeyword) || returnType.ToString().StartsWith("System.Threading.Tasks.Task")
            || (returnType.IsGenericType && returnType.ConstructedFrom.ToString() == "System.Threading.Tasks.Task<TResult>"))
                ReportDiagnostic(context, method);
        }

        private static void ReportDiagnostic(SyntaxNodeAnalysisContext context, MethodDeclarationSyntax method)
        {
            var errorMessage = method.Identifier.ToString() + "Async";
            var diag = Diagnostic.Create(Rule, method.GetLocation(), errorMessage);
            context.ReportDiagnostic(diag);
        }
    }
}