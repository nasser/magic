using System;
using System.Reflection;
using System.Reflection.Emit;
#if NETSTANDARD
using Lokad.ILPack;
#endif

namespace Magic
{
    public static class Emission
    {
        public static AssemblyBuilder DefineDynamicAssembly(AssemblyName name, AssemblyBuilderAccess access)
        {
#if NETSTANDARD
            return AssemblyBuilder.DefineDynamicAssembly(name, access);
#else
            var assy = AppDomain.CurrentDomain.DefineDynamicAssembly(name, access);
            return AppDomain.CurrentDomain.DefineDynamicAssembly(name, access);
#endif
        }

        public static void EmitAssembly(AssemblyBuilder assy, string filename)
        {
#if NETSTANDARD
            var generator = new AssemblyGenerator();
            generator.GenerateAssembly(assy, filename);
#else
            assy.Save(filename);
#endif
        }
    }
}