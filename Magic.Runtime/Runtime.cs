using System;
using System.Reflection;

namespace Magic
{
    public static class Runtime
    {
        public static void InvokeInitType(Assembly assy, Type initType)
        {
            try
            {
                initType.InvokeMember("Initialize", BindingFlags.InvokeMethod | BindingFlags.Static | BindingFlags.Public, Type.DefaultBinder, null, new object[0]);
            }
            catch (Exception e)
            {
                throw new TypeLoadException(String.Format("Error initializing {0}: {1}", assy.FullName, e.Message), e);
            }
        }

        public static string ClojureCLRInitClassName(string sourcePath)
        {
            return "__Init__$" + sourcePath.Replace(".", "/").Replace("/", "$");
        }

        public static bool TryLoadInitType(string relativePath)
        {
            var initClassName = ClojureCLRInitClassName(relativePath);
            Type initType = null;
            foreach (var asm in AppDomain.CurrentDomain.GetAssemblies())
            {
                if (asm is System.Reflection.Emit.AssemblyBuilder)
                    continue;
                initType = asm.GetType(initClassName);
                if (((Object)initType) != null)
                    break;
            }
            if (initType == null)
                return false;

            InvokeInitType(initType.Assembly, initType);
            return true;
        }

        // exists to conform with IL2CPP assumptions about a clean stack after a throw
        public static void ThrowHelper(Exception e)
        {
            throw e;
        }

        public static Type FindType(string p)
        {
            Type t = null;
            // fastest path, will succeed for assembly qualified names (returned by Type.AssemblyQualifiedName)
            // or namespace qualified names (returned by Type.FullName) in the executing assembly or mscorlib
            // e.g. "UnityEngine.Transform, UnityEngine, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null"
            t = Type.GetType(p, false);

            if (((Object)t) != null)
                return t;

            AppDomain domain = AppDomain.CurrentDomain;
            Assembly[] assys = domain.GetAssemblies();

            // fast path, will succeed for namespace qualified names (returned by Type.FullName)
            // e.g. "UnityEngine.Transform"
            for(int i = assys.Length - 1; i>=0; i--)
            {
                var assy = assys[i];
                Type t1 = assy.GetType(p, false);
                if(((Object)t1) != null)
                      return t1;
            }

            // TODO parse name for complex types
            // if (t == null && p.IndexOfAny(_triggerTypeChars) != -1)
            //     t = ClrTypeSpec.GetTypeFromName(p);

            return t;
        }
    }
}