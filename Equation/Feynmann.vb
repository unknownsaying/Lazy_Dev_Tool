Imports System
Imports System.Numerics
Imports System.Threading.Tasks
Imports System.Collections.Generic
Imports System.Linq
Imports System.Windows.Forms
Imports System.Drawing
Imports System.Math

' ============================================
' 费曼路径积分核心类
' ============================================
Public Class FeynmanPathIntegral
    
#Region "物理常数和基本类型"
    Private Const hbar As Double = 1.0545718e-34 ' 约化普朗克常数
    Private Const m As Double = 9.10938356e-31 ' 电子质量
    Private Const kB As Double = 1.380649e-23 ' 玻尔兹曼常数
    
    ' 路径表示：时间点序列
    Public Class Path
        Public Property TimePoints As List(Of Double)
        Public Property PositionPoints As List(Of Double)
        Public Property Action As Complex
        Public Property Weight As Complex
        
        Public Sub New(times As List(Of Double), positions As List(Of Double))
            TimePoints = times
            PositionPoints = positions
        End Sub
        
        Public Function Length() As Integer
            Return Min(TimePoints.Count, PositionPoints.Count)
        End Function
        
        Public Overrides Function ToString() As String
            Return $"路径点: {Length()}, 作用量: {Action.Real:F4} + {Action.Imaginary:F4}i"
        End Function
    End Class
    
    ' 路径积分配置
    Public Class PathIntegralConfig
        Public Property TimeSteps As Integer = 100
        Public Property SpacePoints As Integer = 200
        Public Property Xmin As Double = -10.0
        Public Property Xmax As Double = 10.0
        Public Property TimeTotal As Double = 1.0
        Public Property Potential As Func(Of Double, Double)
        Public Property UseImaginaryTime As Boolean = False ' 虚时间用于统计力学
        
        Public Sub New()
            ' 默认谐振子势
            Potential = Function(x) 0.5 * x * x
        End Sub
    End Class
#End Region

#Region "路径积分计算器"
    Public Class PathIntegralCalculator
        Private config As PathIntegralConfig
        Private random As New Random()
        
        Public Sub New(cfg As PathIntegralConfig)
            config = cfg
        End Sub
        
        ' ============================================
        ' 核心方法1：离散时间近似
        ' ============================================
        Public Function CalculatePropagatorDiscrete(x_i As Double, x_f As Double) As Complex
            Dim N As Integer = config.TimeSteps
            Dim dt As Double = config.TimeTotal / N
            Dim dx As Double = (config.Xmax - config.Xmin) / config.SpacePoints
            
            ' 传播子：K(x_f, t_f; x_i, t_i)
            Dim propagator As Complex = Complex.Zero
            
            ' 使用时间切片公式
            If config.UseImaginaryTime Then
                ' 虚时间（统计力学）
                For k As Integer = 0 To config.SpacePoints - 1
                    Dim x As Double = config.Xmin + k * dx
                    Dim action As Double = CalculateEuclideanAction(x_i, x_f, x, dt)
                    propagator += Exp(-action / hbar) * dx
                Next
                propagator *= Sqrt(m / (2 * PI * hbar * dt))
            Else
                ' 实时（量子力学）
                For k As Integer = 0 To config.SpacePoints - 1
                    Dim x As Double = config.Xmin + k * dx
                    Dim action As Double = CalculateAction(x_i, x_f, x, dt)
                    propagator += Complex.Exp(Complex.ImaginaryOne * action / hbar) * dx
                Next
                propagator *= Complex.Sqrt(New Complex(m / (2 * PI * hbar * dt), 0))
            End If
            
            Return propagator
        End Function
        
        ' ============================================
        ' 核心方法2：蒙特卡洛路径采样
        ' ============================================
        Public Function MonteCarloPaths(x_i As Double, x_f As Double, 
                                       numPaths As Integer, 
                                       numTimeSlices As Integer) As (propagator As Complex, 
                                                                     paths As List(Of Path))
            Dim dt As Double = config.TimeTotal / numTimeSlices
            Dim paths As New List(Of Path)()
            Dim sumWeights As Complex = Complex.Zero
            
            Parallel.For(0, numPaths, Sub(pathIndex)
                Dim path = GenerateRandomPath(x_i, x_f, numTimeSlices)
                Dim action = CalculatePathAction(path, dt)
                
                Dim weight As Complex
                If config.UseImaginaryTime Then
                    weight = Exp(-action / hbar)
                Else
                    weight = Complex.Exp(Complex.ImaginaryOne * action / hbar)
                End If
                
                path.Action = New Complex(action, 0)
                path.Weight = weight
                
                SyncLock paths
                    paths.Add(path)
                    sumWeights += weight
                End SyncLock
            End Sub)
            
            ' 归一化并计算传播子
            Dim propagator As Complex = sumWeights / numPaths
            Dim normalization As Double = Sqrt(m / (2 * PI * hbar * config.TimeTotal))
            propagator *= normalization
            
            Return (propagator, paths)
        End Function
        
        ' ============================================
        ' 核心方法3：递归时间切片
        ' ============================================
        Public Function RecursiveTimeSlicing(x_i As Double, x_f As Double) As Complex
            Return RecursiveIntegrate(x_i, x_f, config.TimeSteps, config.TimeTotal)
        End Function
        
        Private Function RecursiveIntegrate(x_start As Double, x_end As Double, 
                                           slicesRemaining As Integer, 
                                           timeRemaining As Double) As Complex
            If slicesRemaining = 1 Then
                ' 最后一片：直接计算
                Dim dt As Double = timeRemaining
                Dim action As Double = CalculateDirectAction(x_start, x_end, dt)
                
                If config.UseImaginaryTime Then
                    Return Exp(-action / hbar) * 
                           Sqrt(m / (2 * PI * hbar * dt))
                Else
                    Return Complex.Exp(Complex.ImaginaryOne * action / hbar) * 
                           Complex.Sqrt(New Complex(m / (2 * PI * hbar * dt), 0))
                End If
            End If
            
            ' 递归：在中间时间点积分
            Dim dt As Double = timeRemaining / slicesRemaining
            Dim middleTime As Double = dt
            Dim integral As Complex = Complex.Zero
            
            ' 对中间位置积分
            Dim dx As Double = (config.Xmax - config.Xmin) / config.SpacePoints
            
            For k As Integer = 0 To config.SpacePoints - 1
                Dim x_mid As Double = config.Xmin + k * dx
                
                ' 第一段路径的传播子
                Dim K1 As Complex = RecursiveIntegrate(x_start, x_mid, 1, dt)
                
                ' 剩余路径的传播子
                Dim K2 As Complex = RecursiveIntegrate(x_mid, x_end, slicesRemaining - 1, 
                                                      timeRemaining - dt)
                
                integral += K1 * K2 * dx
            Next
            
            Return integral
        End Function
        
        ' ============================================
        ' 作用量计算方法
        ' ============================================
        Private Function CalculateAction(x_i As Double, x_f As Double, 
                                        x_mid As Double, dt As Double) As Double
            ' 离散作用量：S = Σ[ (m/2)(Δx/Δt)^2 - V(x) ] Δt
            Dim v_i As Double = (x_mid - x_i) / dt
            Dim v_f As Double = (x_f - x_mid) / dt
            
            Dim T As Double = 0.5 * m * (v_i * v_i + v_f * v_f) / 2
            Dim V As Double = (config.Potential(x_i) + config.Potential(x_mid) + 
                              config.Potential(x_f)) / 3
            
            Return (T - V) * dt
        End Function
        
        Private Function CalculateEuclideanAction(x_i As Double, x_f As Double, 
                                                 x_mid As Double, dt As Double) As Double
            ' 欧几里得作用量（虚时间）：S_E = ∫[ (m/2)(dx/dτ)^2 + V(x) ] dτ
            Dim v_i As Double = (x_mid - x_i) / dt
            Dim v_f As Double = (x_f - x_mid) / dt
            
            Dim T As Double = 0.5 * m * (v_i * v_i + v_f * v_f) / 2
            Dim V As Double = (config.Potential(x_i) + config.Potential(x_mid) + 
                              config.Potential(x_f)) / 3
            
            Return (T + V) * dt
        End Function
        
        Private Function CalculateDirectAction(x_start As Double, x_end As Double, 
                                              dt As Double) As Double
            ' 直接计算两点间的作用量
            Dim v As Double = (x_end - x_start) / dt
            Dim T As Double = 0.5 * m * v * v
            Dim V As Double = (config.Potential(x_start) + config.Potential(x_end)) / 2
            
            Return (T - V) * dt
        End Function
        
        Private Function CalculatePathAction(path As Path, dt As Double) As Double
            Dim totalAction As Double = 0
            
            For i As Integer = 0 To path.Length() - 2
                Dim x1 As Double = path.PositionPoints(i)
                Dim x2 As Double = path.PositionPoints(i + 1)
                Dim v As Double = (x2 - x1) / dt
                
                Dim T As Double = 0.5 * m * v * v
                Dim V As Double = config.Potential((x1 + x2) / 2)
                
                totalAction += (T - V) * dt
            Next
            
            Return totalAction
        End Function
        
        ' ============================================
        ' 路径生成方法
        ' ============================================
        Private Function GenerateRandomPath(x_i As Double, x_f As Double, 
                                           numSlices As Integer) As Path
            Dim times As New List(Of Double)()
            Dim positions As New List(Of Double)()
            
            Dim dt As Double = config.TimeTotal / numSlices
            
            ' 固定起点和终点
            times.Add(0)
            positions.Add(x_i)
            
            ' 随机生成中间点（布朗桥）
            Dim currentX As Double = x_i
            Dim sigma As Double = Sqrt(hbar * dt / m) ' 量子涨落尺度
            
            For i As Integer = 1 To numSlices - 1
                Dim t As Double = i * dt
                
                ' 布朗运动：随机步进
                Dim randomStep As Double = sigma * (random.NextDouble() - 0.5) * 2
                currentX += randomStep
                
                ' 添加约束：保持接近经典路径
                Dim classicalPath As Double = x_i + (x_f - x_i) * (t / config.TimeTotal)
                currentX = 0.7 * currentX + 0.3 * classicalPath
                
                ' 边界约束
                currentX = Max(config.Xmin, Min(config.Xmax, currentX))
                
                times.Add(t)
                positions.Add(currentX)
            Next
            
            ' 终点
            times.Add(config.TimeTotal)
            positions.Add(x_f)
            
            Return New Path(times, positions)
        End Function
        
        Private Function GenerateClassicalPath(x_i As Double, x_f As Double, 
                                              numPoints As Integer) As Path
            ' 经典路径（最小作用量路径）
            Dim times As New List(Of Double)()
            Dim positions As New List(Of Double)()
            
            For i As Integer = 0 To numPoints - 1
                Dim t As Double = i * config.TimeTotal / (numPoints - 1)
                Dim x As Double = x_i + (x_f - x_i) * (t / config.TimeTotal)
                
                times.Add(t)
                positions.Add(x)
            Next
            
            Return New Path(times, positions)
        End Function
    End Class
#End Region

#Region "势能函数库"
    Public Class Potentials
        ' 自由粒子
        Public Shared Function FreeParticle(x As Double) As Double
            Return 0
        End Function
        
        ' 谐振子
        Public Shared Function HarmonicOscillator(x As Double, Optional omega As Double = 1.0) As Double
            Return 0.5 * omega * omega * x * x
        End Function
        
        ' 双势阱
        Public Shared Function DoubleWell(x As Double, 
                                         Optional a As Double = 1.0, 
                                         Optional b As Double = 1.0) As Double
            Return a * x * x * x * x - b * x * x
        End Function
        
        ' 有限深方势阱
        Public Shared Function FiniteSquareWell(x As Double, 
                                               Optional width As Double = 5.0, 
                                               Optional depth As Double = 10.0) As Double
            If Abs(x) < width / 2 Then
                Return 0
            Else
                Return depth
            End If
        End Function
        
        ' 周期势（晶格）
        Public Shared Function PeriodicPotential(x As Double, 
                                                Optional period As Double = 2 * PI, 
                                                Optional amplitude As Double = 1.0) As Double
            Return amplitude * (1 - Cos(2 * PI * x / period))
        End Function
        
        ' 库仑势
        Public Shared Function CoulombPotential(x As Double, 
                                               Optional charge As Double = 1.0) As Double
            Dim r As Double = Abs(x)
            If r < 1e-10 Then r = 1e-10
            Return -charge / r
        End Function
        
        ' 莫尔斯势（分子振动）
        Public Shared Function MorsePotential(x As Double, 
                                             Optional De As Double = 10.0, 
                                             Optional a As Double = 1.0, 
                                             Optional xe As Double = 0.0) As Double
            Dim r As Double = x - xe
            Return De * (1 - Exp(-a * r)) ^ 2
        End Function
    End Class
#End Region

#Region "路径积分可视化器"
    Public Class PathVisualizer
        Inherits Form
        
        Private WithEvents btnCalculate As New Button()
        Private WithEvents btnAnimate As New Button()
        Private WithEvents cmbPotential As New ComboBox()
        Private WithEvents txtResults As New TextBox()
        Private WithEvents pnlCanvas As New Panel()
        Private WithEvents lblStatus As New Label()
        Private WithEvents numPaths As New NumericUpDown()
        
        Private calculator As PathIntegralCalculator
        Private currentPaths As List(Of Path)
        Private animationTimer As New Timer()
        Private animationStep As Integer = 0
        
        Public Sub New()
            InitializeComponent()
        End Sub
        
        Private Sub InitializeComponent()
            Me.Text = "费曼路径积分模拟器"
            Me.Size = New Size(1200, 800)
            Me.BackColor = Color.FromArgb(20, 20, 30)
            
            ' 控制面板
            Dim controlPanel As New Panel()
            controlPanel.Location = New Point(20, 20)
            controlPanel.Size = New Size(1160, 100)
            controlPanel.BackColor = Color.FromArgb(40, 40, 60)
            
            ' 势能选择
            Dim lblPotential As New Label()
            lblPotential.Text = "势能函数："
            lblPotential.ForeColor = Color.White
            lblPotential.Location = New Point(20, 20)
            
            cmbPotential.Items.AddRange({"自由粒子", "谐振子", "双势阱", "周期势", "库仑势", "莫尔斯势"})
            cmbPotential.SelectedIndex = 1
            cmbPotential.Location = New Point(100, 20)
            cmbPotential.Size = New Size(150, 25)
            
            ' 路径数量
            Dim lblPaths As New Label()
            lblPaths.Text = "路径数量："
            lblPaths.ForeColor = Color.White
            lblPaths.Location = New Point(270, 20)
            
            numPaths.Minimum = 10
            numPaths.Maximum = 10000
            numPaths.Value = 100
            numPaths.Location = New Point(350, 20)
            numPaths.Size = New Size(80, 25)
            
            ' 计算按钮
            btnCalculate.Text = "计算路径积分"
            btnCalculate.Location = New Point(450, 20)
            btnCalculate.Size = New Size(150, 30)
            btnCalculate.BackColor = Color.FromArgb(70, 130, 180)
            btnCalculate.ForeColor = Color.White
            
            ' 动画按钮
            btnAnimate.Text = "路径动画"
            btnAnimate.Location = New Point(620, 20)
            btnAnimate.Size = New Size(120, 30)
            btnAnimate.BackColor = Color.FromArgb(70, 180, 130)
            btnAnimate.ForeColor = Color.White
            
            ' 状态标签
            lblStatus.Location = New Point(20, 60)
            lblStatus.Size = New Size(500, 25)
            lblStatus.ForeColor = Color.LightGreen
            
            controlPanel.Controls.AddRange({lblPotential, cmbPotential, lblPaths, numPaths,
                                          btnCalculate, btnAnimate, lblStatus})
            
            ' 结果文本框
            txtResults.Location = New Point(20, 140)
            txtResults.Size = New Size(400, 600)
            txtResults.Multiline = True
            txtResults.ScrollBars = ScrollBars.Vertical
            txtResults.BackColor = Color.FromArgb(15, 15, 25)
            txtResults.ForeColor = Color.White
            txtResults.Font = New Font("Consolas", 10)
            
            ' 画布面板
            pnlCanvas.Location = New Point(440, 140)
            pnlCanvas.Size = New Size(740, 600)
            pnlCanvas.BackColor = Color.Black
            pnlCanvas.BorderStyle = BorderStyle.FixedSingle
            
            ' 动画定时器
            animationTimer.Interval = 50 ' 20 FPS
            AddHandler animationTimer.Tick, AddressOf AnimationTick
            
            Me.Controls.AddRange({controlPanel, txtResults, pnlCanvas})
        End Sub
        
        Private Sub btnCalculate_Click(sender As Object, e As EventArgs) Handles btnCalculate.Click
            Try
                lblStatus.Text = "正在计算路径积分..."
                Application.DoEvents()
                
                ' 配置路径积分
                Dim config As New PathIntegralConfig()
                config.TimeTotal = 2.0
                config.TimeSteps = 50
                config.SpacePoints = 100
                config.Xmin = -5
                config.Xmax = 5
                
                ' 设置势能函数
                Select Case cmbPotential.SelectedItem.ToString()
                    Case "自由粒子"
                        config.Potential = AddressOf Potentials.FreeParticle
                    Case "双势阱"
                        config.Potential = Function(x) Potentials.DoubleWell(x, 0.1, 1.0)
                    Case "周期势"
                        config.Potential = Function(x) Potentials.PeriodicPotential(x, 2.0, 0.5)
                    Case "库仑势"
                        config.Potential = AddressOf Potentials.CoulombPotential
                    Case "莫尔斯势"
                        config.Potential = Function(x) Potentials.MorsePotential(x, 5.0, 1.0, 0.0)
                    Case Else ' 谐振子
                        config.Potential = Function(x) Potentials.HarmonicOscillator(x, 2.0)
                End Select
                
                ' 创建计算器
                calculator = New PathIntegralCalculator(config)
                
                ' 计算传播子
                Dim x_i As Double = -2.0
                Dim x_f As Double = 2.0
                
                ' 方法1：离散近似
                Dim propagator1 As Complex = calculator.CalculatePropagatorDiscrete(x_i, x_f)
                
                ' 方法2：蒙特卡洛
                Dim result = calculator.MonteCarloPaths(x_i, x_f, CInt(numPaths.Value), 20)
                Dim propagator2 As Complex = result.propagator
                currentPaths = result.paths
                
                ' 显示结果
                DisplayResults(x_i, x_f, propagator1, propagator2)
                
                ' 绘制势能和路径
                DrawPotentialAndPaths()
                
                lblStatus.Text = $"计算完成！生成 {currentPaths.Count} 条路径"
                
            Catch ex As Exception
                MessageBox.Show($"错误: {ex.Message}", "计算错误", 
                              MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End Sub
        
        Private Sub DisplayResults(x_i As Double, x_f As Double, 
                                  propagator1 As Complex, propagator2 As Complex)
            txtResults.Clear()
            txtResults.AppendText("=== 费曼路径积分结果 ===" & vbCrLf & vbCrLf)
            
            txtResults.AppendText($"初始位置: x_i = {x_i:F2}" & vbCrLf)
            txtResults.AppendText($"最终位置: x_f = {x_f:F2}" & vbCrLf)
            txtResults.AppendText($"总时间: t = {2.0:F2}" & vbCrLf & vbCrLf)
            
            txtResults.AppendText("方法1：离散近似" & vbCrLf)
            txtResults.AppendText($"传播子 K = {propagator1.Real:E4} + {propagator1.Imaginary:E4}i" & vbCrLf)
            txtResults.AppendText($"振幅 |K| = {propagator1.Magnitude:E4}" & vbCrLf)
            txtResults.AppendText($"相位 arg(K) = {propagator1.Phase:F4} rad" & vbCrLf & vbCrLf)
            
            txtResults.AppendText("方法2：蒙特卡洛路径采样" & vbCrLf)
            txtResults.AppendText($"传播子 K = {propagator2.Real:E4} + {propagator2.Imaginary:E4}i" & vbCrLf)
            txtResults.AppendText($"振幅 |K| = {propagator2.Magnitude:E4}" & vbCrLf)
            txtResults.AppendText($"相位 arg(K) = {propagator2.Phase:F4} rad" & vbCrLf & vbCrLf)
            
            ' 计算概率
            Dim probability As Double = propagator2.MagnitudeSquared
            txtResults.AppendText($"跃迁概率 P = |K|^2 = {probability:E4}" & vbCrLf & vbCrLf)
            
            ' 路径统计
            If currentPaths IsNot Nothing AndAlso currentPaths.Count > 0 Then
                Dim avgAction As Double = currentPaths.Average(Function(p) p.Action.Real)
                Dim stdAction As Double = Sqrt(currentPaths.Average(
                    Function(p) (p.Action.Real - avgAction) ^ 2))
                
                txtResults.AppendText("路径统计：" & vbCrLf)
                txtResults.AppendText($"平均作用量: {avgAction:F4}" & vbCrLf)
                txtResults.AppendText($"作用量标准差: {stdAction:F4}" & vbCrLf)
                
                ' 找出经典路径（最小作用量）
                Dim classicalPath = currentPaths.OrderBy(Function(p) p.Action.Real).First()
                txtResults.AppendText($"经典路径作用量: {classicalPath.Action.Real:F4}" & vbCrLf)
            End If
        End Sub
        
        Private Sub DrawPotentialAndPaths()
            Dim g As Graphics = pnlCanvas.CreateGraphics()
            g.Clear(Color.Black)
            
            ' 坐标系参数
            Dim width As Integer = pnlCanvas.Width
            Dim height As Integer = pnlCanvas.Height
            Dim margin As Integer = 50
            
            Dim plotWidth As Integer = width - 2 * margin
            Dim plotHeight As Integer = height - 2 * margin
            
            ' 绘制坐标轴
            Dim penAxis As New Pen(Color.White, 2)
            g.DrawLine(penAxis, margin, margin, margin, height - margin) ' Y轴
            g.DrawLine(penAxis, margin, height - margin, width - margin, height - margin) ' X轴
            
            ' 绘制势能函数
            If calculator IsNot Nothing Then
                Dim config = calculator.GetType().GetField("config", 
                    Reflection.BindingFlags.NonPublic Or 
                    Reflection.BindingFlags.Instance).GetValue(calculator)
                
                If config IsNot Nothing Then
                    Dim potential = TryCast(config.GetType().GetProperty("Potential").GetValue(config), 
                                           Func(Of Double, Double))
                    
                    If potential IsNot Nothing Then
                        Dim penPotential As New Pen(Color.Yellow, 2)
                        Dim points As New List(Of Point)()
                        
                        For i As Integer = 0 To plotWidth
                            Dim x_plot As Double = -5 + 10 * i / plotWidth
                            Dim V As Double = potential(x_plot)
                            
                            ' 归一化势能用于显示
                            Dim y_plot As Integer = CInt(height - margin - 
                                                        (V + 5) * plotHeight / 20)
                            points.Add(New Point(margin + i, y_plot))
                        Next
                        
                        If points.Count > 1 Then
                            g.DrawLines(penPotential, points.ToArray())
                        End If
                        
                        penPotential.Dispose()
                    End If
                End If
            End If
            
            ' 绘制路径
            If currentPaths IsNot Nothing AndAlso currentPaths.Count > 0 Then
                ' 按权重排序，绘制最重要的路径
                Dim sortedPaths = currentPaths.OrderByDescending(
                    Function(p) p.Weight.Magnitude).Take(20).ToList()
                
                Dim pathColors() As Color = {
                    Color.FromArgb(100, 255, 0, 0),    ' 红色
                    Color.FromArgb(100, 0, 255, 0),    ' 绿色
                    Color.FromArgb(100, 0, 0, 255),    ' 蓝色
                    Color.FromArgb(100, 255, 255, 0),  ' 黄色
                    Color.FromArgb(100, 255, 0, 255)   ' 洋红色
                }
                
                For pathIndex As Integer = 0 To Min(4, sortedPaths.Count - 1)
                    Dim path = sortedPaths(pathIndex)
                    Dim penPath As New Pen(pathColors(pathIndex), 1)
                    
                    Dim points As New List(Of Point)()
                    
                    For i As Integer = 0 To path.Length() - 1
                        Dim x As Double = path.PositionPoints(i)
                        Dim t As Double = path.TimePoints(i)
                        
                        ' 归一化坐标
                        Dim x_plot As Integer = CInt(margin + (x + 5) * plotWidth / 10)
                        Dim y_plot As Integer = CInt(height - margin - 
                                                    t * plotHeight / 2.0)
                        
                        points.Add(New Point(x_plot, y_plot))
                    Next
                    
                    If points.Count > 1 Then
                        g.DrawLines(penPath, points.ToArray())
                    End If
                    
                    penPath.Dispose()
                Next
                
                ' 绘制经典路径（最小作用量）
                Dim classicalPath = currentPaths.OrderBy(Function(p) p.Action.Real).First()
                Dim penClassical As New Pen(Color.White, 3)
                Dim classicalPoints As New List(Of Point)()
                
                For i As Integer = 0 To classicalPath.Length() - 1
                    Dim x As Double = classicalPath.PositionPoints(i)
                    Dim t As Double = classicalPath.TimePoints(i)
                    
                    Dim x_plot As Integer = CInt(margin + (x + 5) * plotWidth / 10)
                    Dim y_plot As Integer = CInt(height - margin - 
                                                t * plotHeight / 2.0)
                    
                    classicalPoints.Add(New Point(x_plot, y_plot))
                Next
                
                If classicalPoints.Count > 1 Then
                    g.DrawLines(penClassical, classicalPoints.ToArray())
                End If
                
                penClassical.Dispose()
            End If
            
            ' 绘制标签
            Dim brushLabel As New SolidBrush(Color.White)
            g.DrawString("位置 x", New Font("Arial", 12), brushLabel, 
                        width \ 2, height - margin + 20)
            g.DrawString("时间 t", New Font("Arial", 12), brushLabel, 
                        margin - 40, margin)
            
            ' 绘制图例
            Dim legendY As Integer = margin + 10
            g.DrawString("势能函数", New Font("Arial", 10), 
                        New SolidBrush(Color.Yellow), width - 150, legendY)
            legendY += 20
            g.DrawString("经典路径", New Font("Arial", 10), 
                        New SolidBrush(Color.White), width - 150, legendY)
            legendY += 20
            g.DrawString("量子路径", New Font("Arial", 10), 
                        New SolidBrush(Color.Red), width - 150, legendY)
            
            ' 清理资源
            penAxis.Dispose()
            brushLabel.Dispose()
        End Sub
        
        Private Sub AnimationTick(sender As Object, e As EventArgs)
            animationStep += 1
            
            ' 在动画中逐步显示更多路径
            Dim g As Graphics = pnlCanvas.CreateGraphics()
            g.Clear(Color.Black)
            
            DrawPotentialAndPaths()
            
            ' 显示当前步骤
            Dim brush As New SolidBrush(Color.Cyan)
            g.DrawString($"路径积分动画 - 步骤 {animationStep}", 
                        New Font("Arial", 14, FontStyle.Bold), 
                        brush, 100, 20)
            brush.Dispose()
            
            If animationStep > 100 Then
                animationTimer.Stop()
                btnAnimate.Text = "路径动画"
            End If
        End Sub
        
        Private Sub btnAnimate_Click(sender As Object, e As EventArgs) Handles btnAnimate.Click
            If currentPaths Is Nothing OrElse currentPaths.Count = 0 Then
                MessageBox.Show("请先计算路径！")
                Return
            End If
            
            If animationTimer.Enabled Then
                animationTimer.Stop()
                btnAnimate.Text = "路径动画"
            Else
                animationStep = 0
                animationTimer.Start()
                btnAnimate.Text = "停止动画"
            End If
        End Sub
        
        Protected Overrides Sub OnPaint(e As PaintEventArgs)
            MyBase.OnPaint(e)
            DrawPotentialAndPaths()
        End Sub
    End Class
#End Region

#Region "高级功能：量子场论的路径积分"
    Public Class QuantumFieldPathIntegral
        ' 标量场路径积分
        
        Public Class ScalarFieldConfiguration
            Public Property Lattice As Double(,) ' 时空格点上的场值
            Public Property Nx As Integer ' 空间格点数
            Public Property Nt As Integer ' 时间格点数
            Public Property Action As Complex ' 作用量
            
            Public Sub New(nx As Integer, nt As Integer)
                Nx = nx
                Nt = nt
                ReDim Lattice(nt - 1, nx - 1)
            End Sub
        End Class
        
        Public Function CalculateScalarFieldPropagator(mass As Double, 
                                                      coupling As Double,
                                                      latticeSpacing As Double,
                                                      numConfigs As Integer) As Complex
            ' ϕ⁴理论的路径积分
            Dim propagator As Complex = Complex.Zero
            
            Parallel.For(0, numConfigs, Sub(configIndex)
                Dim fieldConfig = GenerateFieldConfiguration(20, 20, mass, coupling)
                Dim action = CalculateFieldAction(fieldConfig, latticeSpacing, mass, coupling)
                Dim weight = Complex.Exp(Complex.ImaginaryOne * action)
                
                SyncLock Me
                    propagator += weight
                End SyncLock
            End Sub)
            
            Return propagator / numConfigs
        End Function
        
        Private Function GenerateFieldConfiguration(Nx As Integer, Nt As Integer,
                                                   mass As Double, 
                                                   coupling As Double) As ScalarFieldConfiguration
            Dim config As New ScalarFieldConfiguration(Nx, Nt)
            Dim random As New Random()
            
            ' 生成随机场配置
            For t As Integer = 0 To Nt - 1
                For x As Integer = 0 To Nx - 1
                    config.Lattice(t, x) = (random.NextDouble() - 0.5) * 2
                Next
            Next
            
            Return config
        End Function
        
        Private Function CalculateFieldAction(config As ScalarFieldConfiguration,
                                             a As Double, ' 格点间距
                                             m As Double, ' 质量
                                             lambda As Double) As Double ' 耦合常数
            Dim action As Double = 0
            
            ' 离散作用量：S = ∫d²x [ 1/2 (∂ϕ)² + 1/2 m²ϕ² + λ/4! ϕ⁴ ]
            For t As Integer = 0 To config.Nt - 1
                For x As Integer = 0 To config.Nx - 1
                    Dim phi As Double = config.Lattice(t, x)
                    
                    ' 动能项 (∂ϕ)²
                    Dim derivative_x As Double = 0
                    Dim derivative_t As Double = 0
                    
                    If x < config.Nx - 1 Then
                        derivative_x = (config.Lattice(t, x + 1) - phi) / a
                    End If
                    
                    If t < config.Nt - 1 Then
                        derivative_t = (config.Lattice(t + 1, x) - phi) / a
                    End If
                    
                    Dim kinetic As Double = 0.5 * (derivative_t * derivative_t - 
                                                  derivative_x * derivative_x)
                    
                    ' 势能项
                    Dim potential As Double = 0.5 * m * m * phi * phi + 
                                            lambda / 24.0 * phi * phi * phi * phi
                    
                    action += (kinetic - potential) * a * a
                Next
            Next
            
            Return action
        End Function
    End Class
#End Region

#Region "使用示例"
    Module MainModule
        <STAThread>
        Sub Main()
            Application.EnableVisualStyles()
            Application.SetCompatibleTextRenderingDefault(False)
            Application.Run(New PathVisualizer())
        End Sub
        
        ' 命令行测试
        Sub TestPathIntegral()
            Console.WriteLine("=== 费曼路径积分测试 ===")
            
            ' 创建配置
            Dim config As New PathIntegralConfig()
            config.TimeSteps = 100
            config.SpacePoints = 200
            config.Xmin = -5
            config.Xmax = 5
            config.TimeTotal = 2.0
            config.Potential = AddressOf Potentials.HarmonicOscillator
            
            ' 创建计算器
            Dim calculator As New PathIntegralCalculator(config)
            
            ' 计算传播子
            Dim x_i As Double = -2.0
            Dim x_f As Double = 2.0
            
            Console.WriteLine("计算离散近似传播子...")
            Dim propagator1 As Complex = calculator.CalculatePropagatorDiscrete(x_i, x_f)
            Console.WriteLine($"K_discrete = {propagator1.Real:E4} + {propagator1.Imaginary:E4}i")
            
            Console.WriteLine(vbCrLf & "计算蒙特卡洛路径积分...")
            Dim result = calculator.MonteCarloPaths(x_i, x_f, 1000, 20)
            Console.WriteLine($"K_MonteCarlo = {result.propagator.Real:E4} + {result.propagator.Imaginary:E4}i")
            Console.WriteLine($"生成路径数: {result.paths.Count}")
            
            ' 计算解析解（自由粒子）
            If config.Potential(0) = 0 Then ' 自由粒子
                Dim t As Double = config.TimeTotal
                Dim analytical As Complex = Complex.Sqrt(
                    New Complex(m / (2 * PI * hbar * t), 0)) * 
                    Complex.Exp(Complex.ImaginaryOne * m * (x_f - x_i) * (x_f - x_i) / (2 * hbar * t))
                Console.WriteLine(vbCrLf & $"解析解: K_analytical = {analytical.Real:E4} + {analytical.Imaginary:E4}i")
            End If
            
            Console.WriteLine(vbCrLf & "测试完成！")
        End Sub
    End Module
#End Region

End Class
