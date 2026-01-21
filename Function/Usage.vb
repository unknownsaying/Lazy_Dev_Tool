Imports System
Imports System.Diagnostics

Module MainModule
    Sub Main()
        Dim waveNorm As New WaveFunctionNormalization()
        Dim stopwatch As New Stopwatch()
        
        Console.WriteLine("=== 概率波归一化计算 ===")
        Console.WriteLine()
        
        ' 方法1：蒙特卡洛积分（适合高维）
        Console.WriteLine("方法1：蒙特卡洛积分")
        stopwatch.Start()
        Dim mcResult As NormalizationResult = waveNorm.MonteCarloNormalization(1000000)
        stopwatch.Stop()
        Console.WriteLine(mcResult.ToString())
        Console.WriteLine($"计算时间: {stopwatch.ElapsedMilliseconds} ms")
        Console.WriteLine()
        
        ' 方法2：梯形法则
        Console.WriteLine("方法2：梯形法则")
        stopwatch.Restart()
        Dim nPoints As Integer = 20
        Dim trapezoidalResult As Double = waveNorm.TrapezoidalNormalization(nPoints)
        stopwatch.Stop()
        Console.WriteLine($"积分值: {trapezoidalResult:E4}")
        Console.WriteLine($"网格点数: {nPoints}^4 = {nPoints^4}")
        Console.WriteLine($"计算时间: {stopwatch.ElapsedMilliseconds} ms")
        Console.WriteLine()
        
        ' 方法3：自适应积分
        Console.WriteLine("方法3：自适应积分")
        stopwatch.Restart()
        Dim adaptiveResult As Double = waveNorm.AdaptiveIntegration(0.0001)
        stopwatch.Stop()
        Console.WriteLine($"积分值: {adaptiveResult:E4}")
        Console.WriteLine($"计算时间: {stopwatch.ElapsedMilliseconds} ms")
        Console.WriteLine()
        
        ' 验证归一化波函数
        Console.WriteLine("归一化验证:")
        Dim normConst As Double = mcResult.NormalizationConstant
        
        ' 计算几个点的归一化波函数值
        For i As Integer = 0 To 2
            Dim x As Double = i * 0.5
            Dim psiNormalized As Complex = waveNorm.NormalizedWaveFunction(x, 0, 0, 0, normConst)
            Console.WriteLine($"Ψ({x},0,0,0) = {psiNormalized.Real:F6} + {psiNormalized.Imaginary:F6}i")
        Next
        
        Console.ReadLine()
    End Sub
End Module