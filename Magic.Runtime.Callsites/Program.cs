using Stubble.Core.Builders;

var callsiteCacheTemplate = File.ReadAllText("CallsiteCache.mustache");
var callsiteInstanceMethodTemplate = File.ReadAllText("CallsiteInstanceMethod.mustache");
var callsiteStaticMethodTemplate = File.ReadAllText("CallsiteStaticMethod.mustache");
var getMethodDelegateTemplate = File.ReadAllText("GetMethodDelegate.mustache");
var stubble = new StubbleBuilder().Build();

GenerateSourceCode();

void GenerateSourceCode(int count=20, string path="out")
{
    Directory.CreateDirectory(path);

    for (var i = 1; i < count; i++)
    {
        File.WriteAllText(Path.Join(path, $"GetMethodDelegate{i:D2}.g.cs"), GenerateGetMethodDelegateMethod(i));
        File.WriteAllText(Path.Join(path, $"CallsiteInstanceMethod{i:D2}.g.cs"), GenerateCallsiteInstanceMethodClass(i));
        File.WriteAllText(Path.Join(path, $"CallsiteStaticMethod{i:D2}.g.cs"), GenerateCallsiteStaticMethodClass(i));
        File.WriteAllText(Path.Join(path, $"CallsiteCacheClass{i:D2}.g.cs"), GenerateCallsiteCacheClass(i));
    }
    File.WriteAllText(Path.Join(path, $"GetMethodDelegate{count:D2}.g.cs"), GenerateGetMethodDelegateMethod(count));
    File.WriteAllText(Path.Join(path, $"CallsiteCacheClass{count:D2}.g.cs"), GenerateCallsiteCacheClass(count));
}

string GenerateGetMethodDelegateMethod(int arity)
{
    var output = stubble.Render(getMethodDelegateTemplate, new
    {
        arityPadded = string.Format("{0:D2}", arity),
        arityPlusOnePadded = string.Format("{0:D2}", arity + 1),
        subscripts = Enumerable.Range(0, arity).ToArray(),
        subscriptsPlusOne = Enumerable.Range(0, arity).Skip(1).ToArray()
    });
    return output.Replace(",)", ")").Replace(" && ;", ";").Replace(",.", ".").Replace(", }", " }").Replace("new [] {  }", "null");
}

string GenerateCallsiteStaticMethodClass(int arity)
{
    var output = stubble.Render(callsiteStaticMethodTemplate, new
    {
        arityPadded = string.Format("{0:D2}", arity),
        subscripts = Enumerable.Range(0, arity).ToArray()
    });
    return output.Replace(",)", ")").Replace(" && ;", ";").Replace(",.", ".").Replace(", }", " }");
}

string GenerateCallsiteInstanceMethodClass(int arity)
{
    var output = stubble.Render(callsiteInstanceMethodTemplate, new
    {
        arityPadded = string.Format("{0:D2}", arity),
        arityPlusOnePadded = string.Format("{0:D2}", arity + 1),
        subscripts = Enumerable.Range(0, arity).ToArray()
    });
    return output.Replace(",)", ")").Replace(" && ;", ";").Replace(",.", ".").Replace(", }", " }");
}

string GenerateCallsiteCacheClass(int arity)
{
    var output = stubble.Render(callsiteCacheTemplate, new
    {
        arityPadded = string.Format("{0:D2}", arity),
        subscripts = Enumerable.Range(0, arity).ToArray()
    });
    return output.Replace(",)", ")").Replace(" && ;", ";");
}