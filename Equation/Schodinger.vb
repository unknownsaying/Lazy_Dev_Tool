Imports System
Imports System.Numerics
Imports System.Windows.Forms
Imports System.Windows.Forms.DataVisualization.Charting
Imports System.Drawing
Imports System.Threading.Tasks
Imports System.Math
' ============================================
' 物理常数类
' ============================================
Public Class PhysicalConstants
    Public Shared ReadOnly Property Hbar As Double = 1.054571817e-34 ' 约化普朗克常数 (J·s)
    Public Shared ReadOnly Property ElectronMass As Double = 9.10938356e-31 ' 电子质量 (kg)
    Public Shared ReadOnly Property ElementaryCharge As Double = 1.60217662e-19 ' 基本电荷 (C)
    Public Shared ReadOnly Property BohrRadius As Double = 5.291772109e-11 ' 玻尔半径 (m)
    
    ' 原子单位制
    Public Shared ReadOnly Property AtomicTime As Double = 2.418884326585e-17 ' 秒
    Public Shared ReadOnly Property AtomicEnergy As Double = 4.3597447222071e-18 ' 焦耳
End Class

' ============================================
' 一维薛定谔方程求解器
' ============================================
Public Class OneDSchrodingerSolver
    Private dx As Double ' 空间步长
    Private dt As Double ' 时间步长
    Private x_min As Double, x_max As Double ' 空间范围
    Private N As Integer ' 网格点数
    Private potential As Func(Of Double, Double) ' 势能函数
    
    ' 物理常数
    Private Const hbar As Double = 1.0 ' 使用原子单位制
    Private Const m As Double = 1.0 ' 电子质量
    
    Public Sub New(xmin As Double, xmax As Double, nPoints As Integer, 
                   potentialFunc As Func(Of Double, Double))
        x_min = xmin
        x_max = xmax
        N = nPoints
        dx = (x_max - x_min) / (N - 1)
        potential = potentialFunc
    End Sub
    
    ' 方法1：虚时间演化求基态
    Public Function FindGroundState(numIterations As Integer) As WaveFunction
        ' 初始化随机波函数
        Dim psi(N - 1) As Complex
        Dim random As New Random()
        
        For i As Integer = 0 To N - 1
            psi(i) = New Complex(random.NextDouble() - 0.5, random.NextDouble() - 0.5)
        Next
        
        ' 归一化
        Normalize(psi)
        
        ' 虚时间演化
        For iter As Integer = 1 To numIterations
            psi = ImaginaryTimeStep(psi, 0.01)
            
            ' 每100步归一化一次
            If iter Mod 100 = 0 Then
                Normalize(psi)
            End If
        Next
        
        ' 最终归一化
        Normalize(psi)
        
        Return New WaveFunction(psi, dx, x_min)
    End Function
    
    ' 方法2：有限差分法求解定态方程
    Public Function SolveStationary(numStates As Integer) As List(Of QuantumState)
        ' 构建哈密顿矩阵
        Dim H(N - 1, N - 1) As Double
        
        For i As Integer = 0 To N - 1
            For j As Integer = 0 To N - 1
                If i = j Then
                    Dim x As Double = x_min + i * dx
                    H(i, j) = hbar ^ 2 / (m * dx ^ 2) + potential(x)
                ElseIf Abs(i - j) = 1 Then
                    H(i, j) = -hbar ^ 2 / (2 * m * dx ^ 2)
                Else
                    H(i, j) = 0
                End If
            Next
        Next
        
        ' 对角化求解本征值和本征态（简化的幂法）
        Return PowerMethod(H, numStates)
    End Function
    
    ' 方法3：含时演化（Crank-Nicolson方法）
    Public Function TimeEvolution(initialPsi As Complex(), totalTime As Double, timeSteps As Integer) As WaveFunction()
        Dim result(timeSteps) As WaveFunction
        dt = totalTime / timeSteps
        
        Dim currentPsi = initialPsi.Clone()
        result(0) = New WaveFunction(currentPsi, dx, x_min)
        
        For t As Integer = 1 To timeSteps
            currentPsi = CrankNicolsonStep(currentPsi)
            result(t) = New WaveFunction(currentPsi, dx, x_min)
        Next
        
        Return result
    End Function
    
    ' ============================================
    ' 核心计算方法
    ' ============================================
    
    ' Crank-Nicolson时间演化步
    Private Function CrankNicolsonStep(psi As Complex()) As Complex()
        Dim newPsi(N - 1) As Complex
        
        ' 构建矩阵 A 和 B (简化版本，使用三对角矩阵算法)
        Dim alpha As Double = Complex.ImaginaryOne * hbar * dt / (4 * m * dx ^ 2)
        
        ' 使用简单的显式方法（更稳定）
        For i As Integer = 1 To N - 2
            Dim x As Double = x_min + i * dx
            Dim V As Double = potential(x)
            
            ' 离散化的拉普拉斯算子
            Dim laplacian As Complex = (psi(i + 1) - 2 * psi(i) + psi(i - 1)) / (dx ^ 2)
            
            ' 时间演化
            newPsi(i) = psi(i) + Complex.ImaginaryOne * dt / hbar * 
                       (-hbar ^ 2 / (2 * m) * laplacian + V * psi(i))
        Next
        
        ' 边界条件（波函数在边界为0）
        newPsi(0) = Complex.Zero
        newPsi(N - 1) = Complex.Zero
        
        Return newPsi
    End Function
    
    ' 虚时间演化步
    Private Function ImaginaryTimeStep(psi As Complex(), timeStep As Double) As Complex()
        Dim newPsi(N - 1) As Complex
        
        For i As Integer = 1 To N - 2
            Dim x As Double = x_min + i * dx
            Dim V As Double = potential(x)
            
            ' 离散化的拉普拉斯算子
            Dim laplacian As Complex = (psi(i + 1) - 2 * psi(i) + psi(i - 1)) / (dx ^ 2)
            
            ' 虚时间演化：τ = it
            newPsi(i) = psi(i) - timeStep * (-hbar ^ 2 / (2 * m) * laplacian + V * psi(i))
        Next
        
        newPsi(0) = Complex.Zero
        newPsi(N - 1) = Complex.Zero
        
        Return newPsi
    End Function
    
    ' 幂法求主要本征态
    Private Function PowerMethod(H As Double(,), numStates As Integer) As List(Of QuantumState)
        Dim states As New List(Of QuantumState)()
        Dim random As New Random()
        
        For stateIndex As Integer = 0 To numStates - 1
            ' 初始随机向量
            Dim v(N - 1) As Double
            For i As Integer = 0 To N - 1
                v(i) = random.NextDouble() - 0.5
            Next
            
            ' 幂法迭代
            For iter As Integer = 1 To 1000
                ' 矩阵乘法
                Dim w(N - 1) As Double
                For i As Integer = 0 To N - 1
                    w(i) = 0
                    For j As Integer = 0 To N - 1
                        w(i) += H(i, j) * v(j)
                    Next
                Next
                
                ' 正交化（相对于已找到的本征态）
                For k As Integer = 0 To states.Count - 1
                    Dim dotProduct As Double = 0
                    For i As Integer = 0 To N - 1
                        dotProduct += w(i) * states(k).WaveFunctionValues(i)
                    Next
                    
                    For i As Integer = 0 To N - 1
                        w(i) -= dotProduct * states(k).WaveFunctionValues(i)
                    Next
                Next
                
                ' 归一化
                Dim norm As Double = Sqrt(w.Select(Function(x) x * x).Sum())
                For i As Integer = 0 To N - 1
                    v(i) = w(i) / norm
                Next
            Next
            
            ' 计算本征值
            Dim Hv(N - 1) As Double
            For i As Integer = 0 To N - 1
                Hv(i) = 0
                For j As Integer = 0 To N - 1
                    Hv(i) += H(i, j) * v(j)
                Next
            Next
            
            Dim eigenvalue As Double = 0
            For i As Integer = 0 To N - 1
                eigenvalue += v(i) * Hv(i)
            Next
            
            ' 转换为复数波函数
            Dim psi(N - 1) As Complex
            For i As Integer = 0 To N - 1
                psi(i) = New Complex(v(i), 0)
            Next
            
            Normalize(psi)
            states.Add(New QuantumState(eigenvalue, psi, dx, x_min))
        Next
        
        Return states.OrderBy(Function(s) s.Energy).ToList()
    End Function
    
    ' 归一化波函数
    Private Sub Normalize(psi As Complex())
        Dim normSquared As Double = 0
        
        For i As Integer = 0 To N - 1
            normSquared += psi(i).MagnitudeSquared() * dx
        Next
        
        Dim norm As Double = Sqrt(normSquared)
        
        If norm > 0 Then
            For i As Integer = 0 To N - 1
                psi(i) = psi(i) / norm
            Next
        End If
    End Sub
End Class

' ============================================
' 数据类：量子态
' ============================================
Public Class QuantumState
    Public Property Energy As Double
    Public Property WaveFunction As WaveFunction
    
    Public ReadOnly Property WaveFunctionValues As Complex()
        Get
            Return WaveFunction.Values
        End Get
    End Property
    
    Public Sub New(e As Double, psi As Complex(), dx As Double, x_min As Double)
        Energy = e
        WaveFunction = New WaveFunction(psi, dx, x_min)
    End Sub
End Class

Public Class WaveFunction
    Public Property Values As Complex()
    Public Property DX As Double
    Public Property Xmin As Double
    
    Public Sub New(psi As Complex(), dx As Double, x_min As Double)
        Values = psi
        Me.DX = dx
        Me.Xmin = x_min
    End Sub
    
    ' 获取概率密度
    Public Function ProbabilityDensity() As Double()
        Dim density(Values.Length - 1) As Double
        
        For i As Integer = 0 To Values.Length - 1
            density(i) = Values(i).MagnitudeSquared()
        Next
        
        Return density
    End Function
    
    ' 获取期望位置
    Public Function ExpectationX() As Double
        Dim sum As Double = 0
        
        For i As Integer = 0 To Values.Length - 1
            Dim x As Double = Xmin + i * DX
            sum += x * Values(i).MagnitudeSquared() * DX
        Next
        
        Return sum
    End Function
End Class

' ============================================
' 势能函数示例
' ============================================
Public Class PotentialFunctions
    ' 无限深方势阱
    Public Shared Function InfiniteSquareWell(x As Double) As Double
        If x < -5 OrElse x > 5 Then
            Return Double.MaxValue
        Else
            Return 0
        End If
    End Function
    
    ' 谐振子势
    Public Shared Function HarmonicOscillator(x As Double) As Double
        Return 0.5 * x * x
    End Function
    
    ' 有限深方势阱
    Public Shared Function FiniteSquareWell(x As Double, width As Double, depth As Double) As Double
        If Abs(x) < width / 2 Then
            Return 0
        Else
            Return depth
        End If
    End Function
    
    ' 库仑势（氢原子）
    Public Shared Function CoulombPotential(x As Double) As Double
        Dim r As Double = Abs(x)
        If r < 0.001 Then
            r = 0.001
        End If
        Return -1.0 / r
    End Function
    
    ' 双势阱
    Public Shared Function DoubleWell(x As Double) As Double
        Return 0.1 * (x * x - 4) ^ 2
    End Function
End Class

' ============================================
' 可视化界面
' ============================================
Public Class QuantumVisualizer
    Inherits Form
    
    Private mainChart As New Chart()
    Private WithEvents btnCalculate As New Button()
    Private WithEvents btnAnimate As New Button()
    Private WithEvents cmbPotential As New ComboBox()
    Private WithEvents txtEnergy As New TextBox()
    Private WithEvents lblStatus As New Label()
    
    Private solver As OneDSchrodingerSolver
    Private currentStates As List(Of QuantumState)
    Private animationTimer As New Timer()
    Private timeStep As Integer = 0
    
    Public Sub New()
        InitializeComponent()
    End Sub
    
    Private Sub InitializeComponent()
        Me.Text = "薛定谔方程量子模拟器"
        Me.Size = New Size(1200, 800)
        Me.BackColor = Color.White
        
        ' 设置图表
        mainChart.Size = New Size(1100, 500)
        mainChart.Location = New Point(50, 150)
        mainChart.BackColor = Color.Black
        mainChart.ForeColor = Color.White
        
        Dim chartArea As New ChartArea()
        chartArea.BackColor = Color.FromArgb(20, 20, 40)
        chartArea.AxisX.Title = "位置 x (原子单位)"
        chartArea.AxisX.TitleForeColor = Color.White
        chartArea.AxisY.Title = "波函数振幅"
        chartArea.AxisY.TitleForeColor = Color.White
        chartArea.AxisX.MajorGrid.LineColor = Color.FromArgb(50, 50, 50)
        chartArea.AxisY.MajorGrid.LineColor = Color.FromArgb(50, 50, 50)
        
        mainChart.ChartAreas.Add(chartArea)
        
        ' 控制面板
        Dim panel As New Panel()
        panel.Location = New Point(50, 20)
        panel.Size = New Size(1100, 120)
        panel.BackColor = Color.FromArgb(30, 30, 60)
        
        ' 势能选择
        Dim lblPotential As New Label()
        lblPotential.Text = "势能函数："
        lblPotential.ForeColor = Color.White
        lblPotential.Location = New Point(20, 20)
        lblPotential.Size = New Size(80, 25)
        
        cmbPotential.Items.AddRange({"无限深方势阱", "谐振子", "库仑势", "双势阱"})
        cmbPotential.SelectedIndex = 0
        cmbPotential.Location = New Point(100, 20)
        cmbPotential.Size = New Size(150, 25)
        
        ' 计算按钮
        btnCalculate.Text = "计算量子态"
        btnCalculate.Location = New Point(270, 20)
        btnCalculate.Size = New Size(120, 30)
        btnCalculate.BackColor = Color.FromArgb(70, 130, 180)
        btnCalculate.ForeColor = Color.White
        
        ' 动画按钮
        btnAnimate.Text = "开始动画"
        btnAnimate.Location = New Point(400, 20)
        btnAnimate.Size = New Size(120, 30)
        btnAnimate.BackColor = Color.FromArgb(70, 180, 130)
        btnAnimate.ForeColor = Color.White
        
        ' 能量显示
        Dim lblEnergy As New Label()
        lblEnergy.Text = "本征能量："
        lblEnergy.ForeColor = Color.White
        lblEnergy.Location = New Point(20, 60)
        
        txtEnergy.Location = New Point(100, 60)
        txtEnergy.Size = New Size(300, 25)
        txtEnergy.ReadOnly = True
        txtEnergy.BackColor = Color.FromArgb(50, 50, 80)
        txtEnergy.ForeColor = Color.Yellow
        
        ' 状态标签
        lblStatus.Location = New Point(20, 90)
        lblStatus.Size = New Size(500, 25)
        lblStatus.ForeColor = Color.LightGreen
        
        panel.Controls.AddRange({lblPotential, cmbPotential, btnCalculate, 
                                btnAnimate, lblEnergy, txtEnergy, lblStatus})
        
        ' 动画定时器
        animationTimer.Interval = 100 ' 100ms
        AddHandler animationTimer.Tick, AddressOf AnimationTick
        
        Me.Controls.Add(panel)
        Me.Controls.Add(mainChart)
    End Sub
    
    Private Sub btnCalculate_Click(sender As Object, e As EventArgs) Handles btnCalculate.Click
        lblStatus.Text = "正在计算量子态..."
        Application.DoEvents()
        
        ' 创建求解器
        Dim potential As Func(Of Double, Double) = GetSelectedPotential()
        solver = New OneDSchrodingerSolver(-10, 10, 500, potential)
        
        ' 求解定态薛定谔方程
        currentStates = solver.SolveStationary(5)
        
        ' 显示结果
        DisplayResults()
        
        lblStatus.Text = $"计算完成！找到 {currentStates.Count} 个量子态"
    End Sub
    
    Private Sub DisplayResults()
        mainChart.Series.Clear()
        
        ' 添加势能曲线
        Dim seriesPotential As New Series("势能")
        seriesPotential.ChartType = SeriesChartType.Line
        seriesPotential.Color = Color.Gray
        seriesPotential.BorderWidth = 2
        
        For i As Integer = 0 To 100
            Dim x As Double = -10 + i * 0.2
            Dim y As Double = GetSelectedPotential()(x)
            seriesPotential.Points.AddXY(x, y)
        Next
        mainChart.Series.Add(seriesPotential)
        
        ' 添加波函数（前3个本征态）
        Dim colors() As Color = {Color.Red, Color.Green, Color.Blue, Color.Cyan, Color.Magenta}
        
        For stateIndex As Integer = 0 To Min(2, currentStates.Count - 1)
            Dim state = currentStates(stateIndex)
            Dim seriesPsi As New Series($"ψ{stateIndex + 1} (E={state.Energy:F3})")
            seriesPsi.ChartType = SeriesChartType.Line
            seriesPsi.Color = colors(stateIndex)
            seriesPsi.BorderWidth = 2
            
            Dim psi = state.WaveFunction
            For i As Integer = 0 To psi.Values.Length - 1 Step 5
                Dim x As Double = psi.Xmin + i * psi.DX
                Dim y As Double = psi.Values(i).Real * 2 + state.Energy ' 偏移显示
                seriesPsi.Points.AddXY(x, y)
            Next
            
            mainChart.Series.Add(seriesPsi)
        Next
        
        ' 更新能量显示
        Dim energyText As String = ""
        For Each state In currentStates
            energyText += $"E{currentStates.IndexOf(state) + 1} = {state.Energy:F3}" + vbCrLf
        Next
        txtEnergy.Text = energyText
        txtEnergy.Multiline = True
        txtEnergy.Height = 80
    End Sub
    
    Private Sub btnAnimate_Click(sender As Object, e As EventArgs) Handles btnAnimate.Click
        If currentStates Is Nothing OrElse currentStates.Count = 0 Then
            MessageBox.Show("请先计算量子态！")
            Return
        End If
        
        If animationTimer.Enabled Then
            animationTimer.Stop()
            btnAnimate.Text = "开始动画"
        Else
            timeStep = 0
            animationTimer.Start()
            btnAnimate.Text = "停止动画"
        End If
    End Sub
    
    Private Sub AnimationTick(sender As Object, e As EventArgs)
        ' 波包动画：叠加前几个本征态并随时间演化
        mainChart.Series.Clear()
        
        ' 添加势能
        Dim seriesPotential As New Series("势能")
        seriesPotential.ChartType = SeriesChartType.Line
        seriesPotential.Color = Color.Gray
        
        For i As Integer = 0 To 100
            Dim x As Double = -10 + i * 0.2
            seriesPotential.Points.AddXY(x, GetSelectedPotential()(x))
        Next
        mainChart.Series.Add(seriesPotential)
        
        ' 创建叠加态
        Dim superposition(499) As Complex
        Dim baseEnergy As Double = currentStates(0).Energy
        
        ' 叠加前三个本征态
        For i As Integer = 0 To superposition.Length - 1
            superposition(i) = Complex.Zero
            
            ' 叠加不同本征态，相位随时间变化
            For stateIndex As Integer = 0 To Min(2, currentStates.Count - 1)
                Dim state = currentStates(stateIndex)
                Dim phase As Double = (state.Energy - baseEnergy) * timeStep * 0.1
                superposition(i) += state.WaveFunction.Values(i) * 
                                   New Complex(Cos(phase), Sin(phase)) * 
                                   (1.0 / (stateIndex + 1))
            Next
        Next
        
        ' 显示概率密度
        Dim seriesProb As New Series("概率密度")
        seriesProb.ChartType = SeriesChartType.Area
        seriesProb.Color = Color.FromArgb(100, 255, 255, 0)
        
        For i As Integer = 0 To superposition.Length - 1 Step 2
            Dim x As Double = -10 + i * 0.04
            Dim prob As Double = superposition(i).MagnitudeSquared()
            seriesProb.Points.AddXY(x, prob * 5) ' 放大显示
        Next
        mainChart.Series.Add(seriesProb)
        
        ' 显示波函数实部
        Dim seriesReal As New Series("波函数(实部)")
        seriesReal.ChartType = SeriesChartType.Line
        seriesReal.Color = Color.Cyan
        seriesReal.BorderWidth = 2
        
        For i As Integer = 0 To superposition.Length - 1 Step 2
            Dim x As Double = -10 + i * 0.04
            seriesReal.Points.AddXY(x, superposition(i).Real + baseEnergy)
        Next
        mainChart.Series.Add(seriesReal)
        
        timeStep += 1
        lblStatus.Text = $"时间步: {timeStep}, 期望位置: {CalculateExpectationX(superposition):F2}"
    End Sub
    
    Private Function GetSelectedPotential() As Func(Of Double, Double)
        Select Case cmbPotential.SelectedItem.ToString()
            Case "谐振子"
                Return AddressOf PotentialFunctions.HarmonicOscillator
            Case "库仑势"
                Return AddressOf PotentialFunctions.CoulombPotential
            Case "双势阱"
                Return AddressOf PotentialFunctions.DoubleWell
            Case Else
                Return AddressOf PotentialFunctions.InfiniteSquareWell
        End Select
    End Function
    
    Private Function CalculateExpectationX(psi As Complex()) As Double
        Dim sum As Double = 0
        Dim norm As Double = 0
        
        For i As Integer = 0 To psi.Length - 1
            Dim x As Double = -10 + i * 0.04
            Dim prob As Double = psi(i).MagnitudeSquared()
            sum += x * prob
            norm += prob
        Next
        
        Return If(norm > 0, sum / norm, 0)
    End Function
End Class

' ============================================
' 主程序入口
' ============================================
Module MainModule
    <STAThread>
    Sub Main()
        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault(False)
        Application.Run(New QuantumVisualizer())
    End Sub

End Module
