namespace Magic.IL2CPP
{
    static class Program
    {
        static void Main(string[] args)
        {
            foreach (var assyPath in args)
            {
                Patches.AnalyzeAssemblyAndWrite(assyPath);
            }
        }
    }
}