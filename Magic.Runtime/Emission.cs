using System;
using System.Reflection.Emit;
using System.Linq;
#if NETSTANDARD
using Lokad.ILPack;
#endif

namespace Magic
{
    public static class Emission
    {
        public static void EmitAssembly(AssemblyBuilder assy, string filename)
        {
#if NETSTANDARD
            Console.WriteLine("[emit] with Lokad.ILPack");
            var generator = new AssemblyGenerator();
            generator.GenerateAssembly(assy, filename);
#else
            Console.WriteLine("[emit] with SRE");
            assy.Save(filename);
#endif
        }
    }
}