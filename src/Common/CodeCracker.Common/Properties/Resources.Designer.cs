﻿//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     Runtime Version:4.0.30319.42000
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

namespace CodeCracker.Properties {
    using System;
    using System.Reflection;
    
    
    /// <summary>
    ///   A strongly-typed resource class, for looking up localized strings, etc.
    /// </summary>
    // This class was auto-generated by the StronglyTypedResourceBuilder
    // class via a tool like ResGen or Visual Studio.
    // To add or remove a member, edit your .ResX file then rerun ResGen
    // with the /str option, or rebuild your VS project.
    [global::System.CodeDom.Compiler.GeneratedCodeAttribute("System.Resources.Tools.StronglyTypedResourceBuilder", "4.0.0.0")]
    [global::System.Diagnostics.DebuggerNonUserCodeAttribute()]
    [global::System.Runtime.CompilerServices.CompilerGeneratedAttribute()]
    public class Resources {
        
        private static global::System.Resources.ResourceManager resourceMan;
        
        private static global::System.Globalization.CultureInfo resourceCulture;
        
        [global::System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
        internal Resources() {
        }
        
        /// <summary>
        ///   Returns the cached ResourceManager instance used by this class.
        /// </summary>
        [global::System.ComponentModel.EditorBrowsableAttribute(global::System.ComponentModel.EditorBrowsableState.Advanced)]
        public static global::System.Resources.ResourceManager ResourceManager {
            get {
                if (object.ReferenceEquals(resourceMan, null)) {
                    global::System.Resources.ResourceManager temp = new global::System.Resources.ResourceManager("CodeCracker.Properties.Resources", typeof(Resources).GetTypeInfo().Assembly);
                    resourceMan = temp;
                }
                return resourceMan;
            }
        }
        
        /// <summary>
        ///   Overrides the current thread's CurrentUICulture property for all
        ///   resource lookups using this strongly typed resource class.
        /// </summary>
        [global::System.ComponentModel.EditorBrowsableAttribute(global::System.ComponentModel.EditorBrowsableState.Advanced)]
        public static global::System.Globalization.CultureInfo Culture {
            get {
                return resourceCulture;
            }
            set {
                resourceCulture = value;
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to String interpolation allows for better reading of the resulting string when compared to Console.WriteLine arguments. You should use Console.WriteLine with arguments only when another method is supplying the format string..
        /// </summary>
        public static string ConsoleWriteLineAnalyzer_Description {
            get {
                return ResourceManager.GetString("ConsoleWriteLineAnalyzer_Description", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Use string interpolation.
        /// </summary>
        public static string ConsoleWriteLineAnalyzer_MessageFormat {
            get {
                return ResourceManager.GetString("ConsoleWriteLineAnalyzer_MessageFormat", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Use string interpolation instead of arguments on Console.WriteLine.
        /// </summary>
        public static string ConsoleWriteLineAnalyzer_Title {
            get {
                return ResourceManager.GetString("ConsoleWriteLineAnalyzer_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Change to string interpolation.
        /// </summary>
        public static string ConsoleWriteLineCodeFixProvider_Title {
            get {
                return ResourceManager.GetString("ConsoleWriteLineCodeFixProvider_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to An empty catch block suppress all errors and shouldn&apos;t be used.\r\nIf the error is expected consider logging it or changing the control flow such that it is explicit..
        /// </summary>
        public static string EmptyCatchBlockAnalyzer_Description {
            get {
                return ResourceManager.GetString("EmptyCatchBlockAnalyzer_Description", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Empty Catch Block..
        /// </summary>
        public static string EmptyCatchBlockAnalyzer_Message {
            get {
                return ResourceManager.GetString("EmptyCatchBlockAnalyzer_Message", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Catch block cannot be empty.
        /// </summary>
        public static string EmptyCatchBlockAnalyzer_Title {
            get {
                return ResourceManager.GetString("EmptyCatchBlockAnalyzer_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Insert Exception class to Catch.
        /// </summary>
        public static string EmptyCatchBlockCodeFixProvider_InsertException {
            get {
                return ResourceManager.GetString("EmptyCatchBlockCodeFixProvider_InsertException", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Remove Empty Catch Block.
        /// </summary>
        public static string EmptyCatchBlockCodeFixProvider_Remove {
            get {
                return ResourceManager.GetString("EmptyCatchBlockCodeFixProvider_Remove", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Remove Empty Catch Block and Put a Documentation Link about Try...Catch use.
        /// </summary>
        public static string EmptyCatchBlockCodeFixProvider_RemoveAndDocumentation {
            get {
                return ResourceManager.GetString("EmptyCatchBlockCodeFixProvider_RemoveAndDocumentation", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Remove wrapping Try Block.
        /// </summary>
        public static string EmptyCatchBlockCodeFixProvider_RemoveTry {
            get {
                return ResourceManager.GetString("EmptyCatchBlockCodeFixProvider_RemoveTry", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Change field type &apos;{0}&apos; accessibility to be as accessible as field &apos;{1}&apos;.
        /// </summary>
        public static string InconsistentAccessibilityInFieldType_Title {
            get {
                return ResourceManager.GetString("InconsistentAccessibilityInFieldType_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Change parameter type &apos;{0}&apos; accessibility to be as accessible as indexer &apos;this[{1}]&apos;.
        /// </summary>
        public static string InconsistentAccessibilityInIndexerParameter_Title {
            get {
                return ResourceManager.GetString("InconsistentAccessibilityInIndexerParameter_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Change indexer return type &apos;{0}&apos; accessibility to be as accessible as indexer &apos;this[{1}]&apos;.
        /// </summary>
        public static string InconsistentAccessibilityInIndexerReturnType_Title {
            get {
                return ResourceManager.GetString("InconsistentAccessibilityInIndexerReturnType_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Change parameter type &apos;{0}&apos; accessibility to be as accessible as method &apos;{1}&apos;.
        /// </summary>
        public static string InconsistentAccessibilityInMethodParameter_Title {
            get {
                return ResourceManager.GetString("InconsistentAccessibilityInMethodParameter_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Change return type &apos;{0}&apos; accessibility to be as accessible as method &apos;{1}&apos;.
        /// </summary>
        public static string InconsistentAccessibilityInMethodReturnType_Title {
            get {
                return ResourceManager.GetString("InconsistentAccessibilityInMethodReturnType_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Change property type &apos;{0}&apos; accessibility to be as accessible as property &apos;{1}&apos;.
        /// </summary>
        public static string InconsistentAccessibilityInPropertyType_Title {
            get {
                return ResourceManager.GetString("InconsistentAccessibilityInPropertyType_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Consider introduce field for constructor parameters..
        /// </summary>
        public static string IntroduceFieldFromConstructorAnalyzer_Description {
            get {
                return ResourceManager.GetString("IntroduceFieldFromConstructorAnalyzer_Description", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Introduce a field for parameter: {0}.
        /// </summary>
        public static string IntroduceFieldFromConstructorAnalyzer_MessageFormat {
            get {
                return ResourceManager.GetString("IntroduceFieldFromConstructorAnalyzer_MessageFormat", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Consider introduce field for constructor parameters..
        /// </summary>
        public static string IntroduceFieldFromConstructorAnalyzer_Title {
            get {
                return ResourceManager.GetString("IntroduceFieldFromConstructorAnalyzer_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Introduce field: {0} from constructor..
        /// </summary>
        public static string IntroduceFieldFromConstructorCodeFixProvider_MessageFormat {
            get {
                return ResourceManager.GetString("IntroduceFieldFromConstructorCodeFixProvider_MessageFormat", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Introduce fields for constructor parameters..
        /// </summary>
        public static string IntroduceFieldFromConstructorCodeFixProvider_Title {
            get {
                return ResourceManager.GetString("IntroduceFieldFromConstructorCodeFixProvider_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Make method non async.
        /// </summary>
        public static string MakeMethodNonAsyncCodeFixProvider_Title {
            get {
                return ResourceManager.GetString("MakeMethodNonAsyncCodeFixProvider_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to In C#6 the nameof() operator should be used to specify the name of a program element instead of a string literal as it produce code that is easier to refactor..
        /// </summary>
        public static string NameOfAnalyzer_Description {
            get {
                return ResourceManager.GetString("NameOfAnalyzer_Description", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Use &apos;nameof({0})&apos; instead of specifying the program element name..
        /// </summary>
        public static string NameOfAnalyzer_MessageFormat {
            get {
                return ResourceManager.GetString("NameOfAnalyzer_MessageFormat", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Use nameof.
        /// </summary>
        public static string NameOfAnalyzer_Title {
            get {
                return ResourceManager.GetString("NameOfAnalyzer_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Use nameof().
        /// </summary>
        public static string NameOfCodeFixProvider_Title {
            get {
                return ResourceManager.GetString("NameOfCodeFixProvider_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Create static PropertyChangedEventArgs instance and reuse.
        /// </summary>
        public static string PropertyChangedEventArgsUnnecessaryAllocation_CodeActionTitle {
            get {
                return ResourceManager.GetString("PropertyChangedEventArgsUnnecessaryAllocation_CodeActionTitle", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Creating every time an instance of PropertyChangedEventArgs class causes unnecessary memory allocation. Instance can be created once and reused..
        /// </summary>
        public static string PropertyChangedEventArgsUnnecessaryAllocation_Description {
            get {
                return ResourceManager.GetString("PropertyChangedEventArgsUnnecessaryAllocation_Description", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Create PropertyChangedEventArgs static instance and reuse it to avoid unecessary memory allocation..
        /// </summary>
        public static string PropertyChangedEventArgsUnnecessaryAllocation_MessageFormat {
            get {
                return ResourceManager.GetString("PropertyChangedEventArgsUnnecessaryAllocation_MessageFormat", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to PropertyChangedEventArgs unnecessary allocation.
        /// </summary>
        public static string PropertyChangedEventArgsUnnecessaryAllocation_Title {
            get {
                return ResourceManager.GetString("PropertyChangedEventArgsUnnecessaryAllocation_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Getter only properties with backing read-only field can be converted to getter-only auto-properties..
        /// </summary>
        public static string ReplaceWithGetterOnlyAutoPropertyAnalyzer_Description {
            get {
                return ResourceManager.GetString("ReplaceWithGetterOnlyAutoPropertyAnalyzer_Description", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Property {0} can be converted to an getter-only auto-property..
        /// </summary>
        public static string ReplaceWithGetterOnlyAutoPropertyAnalyzer_MessageFormat {
            get {
                return ResourceManager.GetString("ReplaceWithGetterOnlyAutoPropertyAnalyzer_MessageFormat", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Property can be simplified by using an getter-only auto-property..
        /// </summary>
        public static string ReplaceWithGetterOnlyAutoPropertyAnalyzer_Title {
            get {
                return ResourceManager.GetString("ReplaceWithGetterOnlyAutoPropertyAnalyzer_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Simplify by using an getter-only auto-property.
        /// </summary>
        public static string ReplaceWithGetterOnlyAutoPropertyCodeFixProvider_Title {
            get {
                return ResourceManager.GetString("ReplaceWithGetterOnlyAutoPropertyCodeFixProvider_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to String interpolation allows for better reading of the resulting string when compared to String.Format. You should use String.Format only when another method is supplying the format string..
        /// </summary>
        public static string StringFormatAnalyzer_Description {
            get {
                return ResourceManager.GetString("StringFormatAnalyzer_Description", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Use string interpolation.
        /// </summary>
        public static string StringFormatAnalyzer_MessageFormat {
            get {
                return ResourceManager.GetString("StringFormatAnalyzer_MessageFormat", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Use string interpolation instead of String.Format.
        /// </summary>
        public static string StringFormatAnalyzer_Title {
            get {
                return ResourceManager.GetString("StringFormatAnalyzer_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Change to string interpolation.
        /// </summary>
        public static string StringFormatCodeFixProvider_Title {
            get {
                return ResourceManager.GetString("StringFormatCodeFixProvider_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Auto properties offer a more concise way of defining a property. If you are using simple getters and setters you are able to simplify your code with autoproperties..
        /// </summary>
        public static string SwitchToAutoPropAnalyzer_Description {
            get {
                return ResourceManager.GetString("SwitchToAutoPropAnalyzer_Description", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Change {0} to an auto property.
        /// </summary>
        public static string SwitchToAutoPropAnalyzer_MessageFormat {
            get {
                return ResourceManager.GetString("SwitchToAutoPropAnalyzer_MessageFormat", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Use auto property.
        /// </summary>
        public static string SwitchToAutoPropAnalyzer_Title {
            get {
                return ResourceManager.GetString("SwitchToAutoPropAnalyzer_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Change to auto property.
        /// </summary>
        public static string SwitchToAutoPropCodeFixProvider_Title {
            get {
                return ResourceManager.GetString("SwitchToAutoPropCodeFixProvider_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to You have missing/unexistent parameters in Xml Docs.
        /// </summary>
        public static string XmlDocumentationAnalyzer_Title {
            get {
                return ResourceManager.GetString("XmlDocumentationAnalyzer_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Create missing parameters in xml docs.
        /// </summary>
        public static string XmlDocumentationCreateMissingParametersCodeFixProvider_Title {
            get {
                return ResourceManager.GetString("XmlDocumentationCreateMissingParametersCodeFixProvider_Title", resourceCulture);
            }
        }
        
        /// <summary>
        ///   Looks up a localized string similar to Remove unexistent parameters in xml docs.
        /// </summary>
        public static string XmlDocumentationRemoveNonExistentParametersCodeFixProvider_Title {
            get {
                return ResourceManager.GetString("XmlDocumentationRemoveNonExistentParametersCodeFixProvider_Title", resourceCulture);
            }
        }
    }
}
