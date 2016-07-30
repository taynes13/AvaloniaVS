using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TestHelper;

namespace AvaloniaAnalyzers.Test
{
    [TestClass]
    public class DependencyPropertyConverterAnalyzerTests : DiagnosticVerifier
    {
        protected override DiagnosticAnalyzer GetCSharpDiagnosticAnalyzer()
        {
            return new DependencyPropertyToAvaloniaPropertyAnalyzer();
        }


        //No diagnostics expected to show up
        [TestMethod]
        public void NoDiagnosticOnEmptyInput()
        {
            var test = @"";

            VerifyCSharpDiagnostic(test);
        }
        
        [TestMethod]
        public void DiagnosticTriggeredOnDependencyProperty()
        {
            var test = @"
    using System.Windows;

    namespace ConsoleApplication1
    {
        class TestProperty
        {
            public static readonly DependencyProperty Property1 = DependencyProperty.Register(""Property"", typeof(int), typeof(TestProperty));
        }
    }";
            var expected = new DiagnosticResult
            {
                Id = DependencyPropertyToAvaloniaPropertyAnalyzer.DiagnosticId,
                Message = "Type name '{0}' contains lowercase letters",
                Severity = DiagnosticSeverity.Warning,
                Locations =
                    new[] {
                            new DiagnosticResultLocation("Test0.cs", 8, 55)
                        }
            };

            VerifyCSharpDiagnostic(test, expected);
        }
    }
}
