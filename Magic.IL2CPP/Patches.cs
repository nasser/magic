using System;
using System.IO;
using System.Collections.Generic;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;

namespace Magic.IL2CPP
{
    public static class Patches
    {
        public static void AnalyzeAssemblyAndWrite(string file)
        {
            var outfile = file + ".out";
            using(var assyFile = File.OpenRead(file))
            {
                var assy = AssemblyDefinition.ReadAssembly(file);
                Console.WriteLine(assy.FullName);
                foreach (var t in assy.MainModule.Types)
                {
                    foreach (var m in t.Methods)
                    {
                        if (m.HasBody)
                            AnalyzeMethod(m);
                    }
                }
                assy.MainModule.Write(outfile);
            }
            File.Delete(file);
            File.Move(outfile, file);
            File.Delete(outfile);
        }

        public static void AnalyzeMethod(MethodDefinition m)
        {
            m.Body.SimplifyMacros();

            EliminateUnreachableBranches(m);

            m.Body.OptimizeMacros();
        }

        public static void EliminateUnreachableBranches(MethodDefinition m)
        {
            var branchTargets = new HashSet<Instruction>();
            var potentiallyUnreachableBranches = new HashSet<Instruction>();
            var removedBranches = false;

            var lastInstruction = m.Body.Instructions[0];
            foreach (var i in m.Body.Instructions)
            {
                if (i.OpCode == OpCodes.Br
                   || i.OpCode == OpCodes.Brfalse
                   || i.OpCode == OpCodes.Brtrue)
                    branchTargets.Add((Instruction)i.Operand);

                if (i.OpCode == OpCodes.Br && (lastInstruction.OpCode == OpCodes.Br || lastInstruction.OpCode == OpCodes.Throw))
                    potentiallyUnreachableBranches.Add(i);
                lastInstruction = i;
            }

            foreach (var i in potentiallyUnreachableBranches)
            {
                if (!branchTargets.Contains(i))
                {
                    Console.WriteLine("[{1}] removing unreachable branch ({0})", i, m);
                    m.Body.Instructions.Remove(i);
                    removedBranches = true;
                }
            }

            if(removedBranches)
                EliminateUnreachableBranches(m);
        }
    }
}