using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TestHelper;

namespace AvaloniaAnalyzers.Test
{
    [TestClass]
    public class DependencyPropertyConverterFixerTests : CodeFixVerifier
    {
        protected override DiagnosticAnalyzer GetCSharpDiagnosticAnalyzer()
        {
            return new DependencyPropertyToAvaloniaPropertyAnalyzer();
        }

        protected override CodeFixProvider GetCSharpCodeFixProvider()
        {
            return new DependencyPropertyToAvaloniaPropertyFixer();
        }

        [TestMethod]
        public void ConvertsBasicDependencyPropertyToAvaloniaProperty()
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
            var fixTest = @"
    using System.Windows;

    namespace ConsoleApplication1
    {
        class TestProperty
        {
            public static readonly Avalonia.StyledProperty<System.Int32> Property1 = Avalonia.AvaloniaProperty.Register<TestProperty, int>(""Property"");
        }
    }";
            VerifyCSharpFix(test, fixTest, allowNewCompilerDiagnostics: true);
        }

        [TestMethod]
        public void ConvertsReadOnlyDependencyPropertyToAvaloniaProperty()
        {
            var test = @"
    using System.Windows;

    namespace ConsoleApplication1
    {
        class TestProperty
        {
            public static readonly DependencyProperty Property1 = DependencyProperty.RegisterReadOnly(""Property"", typeof(int), typeof(TestProperty));
        }
    }";
            var fixTest = @"
    using System.Windows;

    namespace ConsoleApplication1
    {
        class TestProperty
        {
            public static readonly Avalonia.StyledProperty<System.Int32> Property1 = Avalonia.AvaloniaProperty.Register<TestProperty, int>(""Property"");
        }
    }";
            VerifyCSharpFix(test, fixTest, allowNewCompilerDiagnostics: true);
        }

        [TestMethod]
        public void ConvertsAttachedDependencyPropertyToAttachedAvaloniaProperty()
        {
            var test = @"
    using System.Windows;

    namespace ConsoleApplication1
    {
        class TestProperty
        {
            public static readonly DependencyProperty Property1 = DependencyProperty.RegisterAttached(""Property"", typeof(int), typeof(OtherType));
        }
        class OtherType {}
    }";
            var fixTest = @"
    using System.Windows;

    namespace ConsoleApplication1
    {
        class TestProperty
        {
            public static readonly Avalonia.AttachedProperty<System.Int32> Property1 = Avalonia.AvaloniaProperty.RegisterAttached<TestProperty, OtherType, int>(""Property"");
        }
        class OtherType {}
    }";
            VerifyCSharpFix(test, fixTest, allowNewCompilerDiagnostics: true);
        }

        [TestMethod]
        public void MovesOnChangedHandlerToStaticConstructor()
        {
            var test = @"
using System.Windows;

class TestWithHandler
{
    public static readonly DependencyProperty Property1 = DependencyProperty.Register(""Property1"", typeof(int), typeof(TestWithHandler), new PropertyMetadata(Changed));

    private static void Changed(DependencyObject o, object v){}
}";
            var fixTest = @"
using System.Windows;

class TestWithHandler
{
    public static readonly Avalonia.StyledProperty<System.Int32> Property1 = Avalonia.AvaloniaProperty.Register<TestWithHandler, int>(""Property1"");

    private static void Changed(DependencyObject o, object v){}

    static TestWithHandler()
    {
        Property1.Changed.AddClassHandler<TestWithHandler>(Changed);
    }
}";
            VerifyCSharpFix(test, fixTest, allowNewCompilerDiagnostics: true);
        }

        [TestMethod]
        public void PreservesDefaultValue()
        {
            var test = @"
using System.Windows;

class TestWithHandler
{
    public static readonly DependencyProperty Property1 = DependencyProperty.Register(""Property1"", typeof(int), typeof(TestWithHandler), new PropertyMetadata(5));
}";
            var fixTest = @"
using System.Windows;

class TestWithHandler
{
    public static readonly Avalonia.StyledProperty<System.Int32> Property1 = Avalonia.AvaloniaProperty.Register<TestWithHandler, int>(""Property1"", 5);
}";
            VerifyCSharpFix(test, fixTest, allowNewCompilerDiagnostics: true);
        }

        [TestMethod]
        public void InheritanceMetadataPreserved()
        {
            var test = @"
using System.Windows;

class TestWithHandler
{
    public static readonly DependencyProperty Property1 = DependencyProperty.Register(""Property1"", typeof(int), typeof(TestWithHandler), new FrameworkPropertyMetadata(5, FrameworkPropertyMetadataOptions.Inherits));
}";
            var fixTest = @"
using System.Windows;

class TestWithHandler
{
    public static readonly Avalonia.StyledProperty<System.Int32> Property1 = Avalonia.AvaloniaProperty.Register<TestWithHandler, int>(""Property1"", 5, inherits: true);
}";
            VerifyCSharpFix(test, fixTest, allowNewCompilerDiagnostics: true);
        }

        [TestMethod]
        public void DefaultBindingModePreserved()
        {
            var test = @"
using System.Windows;

class TestWithHandler
{
    public static readonly DependencyProperty Property1 = DependencyProperty.Register(""Property1"", typeof(int), typeof(TestWithHandler), new FrameworkPropertyMetadata(5, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));
}";
            var fixTest = @"
using System.Windows;

class TestWithHandler
{
    public static readonly Avalonia.StyledProperty<System.Int32> Property1 = Avalonia.AvaloniaProperty.Register<TestWithHandler, int>(""Property1"", 5, defaultBindingMode: Avalonia.Data.BindingMode.TwoWay);
}";
            VerifyCSharpFix(test, fixTest, allowNewCompilerDiagnostics: true);
        }
        
        [Ignore] // Currently cannot resolve at compile time because of the bitwise or (and maybe some issues with the test harness setup)
        [TestMethod]
        public void DefaultBindingModeAndInheritsPreserved()
        {
            var test = @"
using System.Windows;

class TestWithHandler
{
    public static readonly DependencyProperty Property1 = DependencyProperty.Register(""Property1"", typeof(int), typeof(TestWithHandler), new FrameworkPropertyMetadata(5, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault | FrameworkPropertyMetadataOptions.Inherits));
}";
            var fixTest = @"
using System.Windows;

class TestWithHandler
{
    public static readonly Avalonia.StyledProperty<System.Int32> Property1 = Avalonia.AvaloniaProperty.Register<TestWithHandler, int>(""Property1"", 5, inherits: true, defaultBindingMode: Avalonia.Data.BindingMode.TwoWay);
}";
            VerifyCSharpFix(test, fixTest, allowNewCompilerDiagnostics: true);
        }
    }
}
