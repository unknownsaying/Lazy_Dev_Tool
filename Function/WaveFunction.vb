Imports System
Imports System.Math
Imports dxdydzdt.PNG
Public Class WaveFunctionNormalization
    
    ' 定义四维波函数（示例：高斯波包）
    Public Function WaveFunction(x As Double, y As Double, z As Double, t As Double) As Complex
        ' 示例：高斯波包
        Dim sigma_x As Double = 1.0
        Dim sigma_y As Double = 1.0
        Dim sigma_z As Double = 1.0
        Dim omega As Double = 2.0
        
        Dim amplitude As Double = Exp(-(x^2/(2*sigma_x^2) + y^2/(2*sigma_y^2) + z^2/(2*sigma_z^2)))
        Dim phase As Double = Cos(omega * t)
        
        Return New Complex(amplitude * phase, 0)
    End Function
    
    ' 蒙特卡洛方法进行四重积分（适合高维）
    Public Function MonteCarloNormalization(iterations As Integer) As NormalizationResult
        Dim random As New Random()
        Dim sum As Double = 0
        Dim sumSquared As Double = 0
        Dim volume As Double = 64.0 ' 积分区域体积: [-2,2]^4
        
        For i As Integer = 1 To iterations
            ' 在积分区域内随机采样
            Dim x As Double = (random.NextDouble() - 0.5) * 4 ' [-2, 2]
            Dim y As Double = (random.NextDouble() - 0.5) * 4
            Dim z As Double = (random.NextDouble() - 0.5) * 4
            Dim t As Double = (random.NextDouble() - 0.5) * 4
            
            Dim psi As Complex = WaveFunction(x, y, z, t)
            Dim probabilityDensity As Double = psi.MagnitudeSquared()
            
            sum += probabilityDensity
            sumSquared += probabilityDensity * probabilityDensity
        Next
        
        ' 计算积分值和误差估计
        Dim average As Double = sum / iterations
        Dim integral As Double = average * volume
        
        ' 误差估计
        Dim variance As Double = (sumSquared / iterations - average * average)
        Dim errorEstimate As Double = volume * Sqrt(variance / iterations)
        
        Return New NormalizationResult(integral, errorEstimate)
    End Function
    
    ' 梯形法则进行四重积分（规则网格）
    Public Function TrapezoidalNormalization(n As Integer) As Double
        Dim a As Double = -2.0 ' 积分下限
        Dim b As Double = 2.0  ' 积分上限
        
        Dim h As Double = (b - a) / n
        Dim sum As Double = 0
        
        For i As Integer = 0 To n
            Dim x As Double = a + i * h
            For j As Integer = 0 To n
                Dim y As Double = a + j * h
                For k As Integer = 0 To n
                    Dim z As Double = a + k * h
                    For l As Integer = 0 To n
                        Dim t As Double = a + l * h
                        
                        Dim psi As Complex = WaveFunction(x, y, z, t)
                        Dim weight As Double = 1.0
                        
                        ' 端点权重调整
                        If i = 0 OrElse i = n Then weight *= 0.5
                        If j = 0 OrElse j = n Then weight *= 0.5
                        If k = 0 OrElse k = n Then weight *= 0.5
                        If l = 0 OrElse l = n Then weight *= 0.5
                        
                        sum += weight * psi.MagnitudeSquared()
                    Next
                Next
            Next
        Next
        
        Dim integral As Double = sum * (h ^ 4)
        Return integral
    End Function
    
    ' 自适应积分法（更精确）
    Public Function AdaptiveIntegration(epsilon As Double) As Double
        Dim integrationRegion As New IntegrationRegion(-2, 2, -2, 2, -2, 2, -2, 2)
        Return AdaptiveIntegrate(integrationRegion, epsilon)
    End Function
    
    Private Function AdaptiveIntegrate(region As IntegrationRegion, epsilon As Double) As Double
        ' 计算区域中心的函数值
        Dim centerValue As Double = EvaluateRegion(region)
        
        ' 如果区域足够小，返回近似值
        If region.Volume < epsilon Then
            Return centerValue * region.Volume
        End If
        
        ' 否则分割区域并递归
        Dim subRegions As List(Of IntegrationRegion) = region.Split()
        Dim sum As Double = 0
        
        For Each subRegion As IntegrationRegion In subRegions
            sum += AdaptiveIntegrate(subRegion, epsilon / 16)
        Next
        
        Return sum
    End Function
    
    Private Function EvaluateRegion(region As IntegrationRegion) As Double
        ' 计算区域中心点的概率密度
        Dim x As Double = (region.Xmin + region.Xmax) / 2
        Dim y As Double = (region.Ymin + region.Ymax) / 2
        Dim z As Double = (region.Zmin + region.Zmax) / 2
        Dim t As Double = (region.Tmin + region.Tmax) / 2
        
        Dim psi As Complex = WaveFunction(x, y, z, t)
        Return psi.MagnitudeSquared()
    End Function
    
    ' 归一化波函数
    Public Function NormalizedWaveFunction(x As Double, y As Double, z As Double, t As Double, normConstant As Double) As Complex
        Dim psi As Complex = WaveFunction(x, y, z, t)
        Return psi / Sqrt(normConstant)
    End Function
End Class

' 辅助类：复数
Public Structure Complex
    Public Real As Double
    Public Imaginary As Double
    
    Public Sub New(r As Double, i As Double)
        Real = r
        Imaginary = i
    End Sub
    
    Public ReadOnly Property MagnitudeSquared() As Double
        Get
            Return Real * Real + Imaginary * Imaginary
        End Get
    End Property
    
    Public ReadOnly Property Magnitude() As Double
        Get
            Return Sqrt(MagnitudeSquared)
        End Get
    End Property
    
    Public Shared Operator /(c1 As Complex, d As Double) As Complex
        Return New Complex(c1.Real / d, c1.Imaginary / d)
    End Operator
End Structure

' 辅助类：积分区域
Public Class IntegrationRegion
    Public Xmin, Xmax, Ymin, Ymax, Zmin, Zmax, Tmin, Tmax As Double
    
    Public Sub New(x1 As Double, x2 As Double, y1 As Double, y2 As Double, 
                   z1 As Double, z2 As Double, t1 As Double, t2 As Double)
        Xmin = x1 : Xmax = x2
        Ymin = y1 : Ymax = y2
        Zmin = z1 : Zmax = z2
        Tmin = t1 : Tmax = t2
    End Sub
    
    Public ReadOnly Property Volume() As Double
        Get
            Return (Xmax - Xmin) * (Ymax - Ymin) * (Zmax - Zmin) * (Tmax - Tmin)
        End Get
    End Property
    
    Public Function Split() As List(Of IntegrationRegion)
        Dim regions As New List(Of IntegrationRegion)
        
        Dim xMid As Double = (Xmin + Xmax) / 2
        Dim yMid As Double = (Ymin + Ymax) / 2
        Dim zMid As Double = (Zmin + Zmax) / 2
        Dim tMid As Double = (Tmin + Tmax) / 2
        
        ' 分割为16个子区域（四维中每个维度分为2份）
        For xi As Integer = 0 To 1
            For yi As Integer = 0 To 1
                For zi As Integer = 0 To 1
                    For ti As Integer = 0 To 1
                        Dim newRegion As New IntegrationRegion(
                            If(xi = 0, Xmin, xMid), If(xi = 0, xMid, Xmax),
                            If(yi = 0, Ymin, yMid), If(yi = 0, yMid, Ymax),
                            If(zi = 0, Zmin, zMid), If(zi = 0, zMid, Zmax),
                            If(ti = 0, Tmin, tMid), If(ti = 0, tMid, Tmax))
                        regions.Add(newRegion)
                    Next
                Next
            Next
        Next
        
        Return regions
    End Function
End Class

' 结果类
Public Class NormalizationResult
    Public Property IntegralValue As Double
    Public Property ErrorEstimate As Double
    Public Property NormalizationConstant As Double
    
    Public Sub New(integral As Double, errorEst As Double)
        IntegralValue = integral
        ErrorEstimate = errorEst
        NormalizationConstant = integral
    End Sub
    
    Public Overrides Function ToString() As String
        Return $"归一化积分值: {IntegralValue:E4} ± {ErrorEstimate:E4}" & 
               $"{vbCrLf}归一化常数: {Sqrt(NormalizationConstant):E4}"
    End Function
End Class