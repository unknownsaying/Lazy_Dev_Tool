Imports System
Imports System.Numerics
Imports System.Windows.Forms
Imports System.Drawing
Imports System.Threading.Tasks

' ============================================
' 矩阵量子力学核心类
' ============================================
Public Class MatrixQuantumMechanics
    
#Region "物理常数"
    Private Const hbar As Double = 1.0 ' 约化普朗克常数（原子单位）
    Private Const m As Double = 1.0 ' 粒子质量
#End Region

#Region "矩阵运算基础"
    ' 复数矩阵类
    Public Class ComplexMatrix
        Private data(,) As Complex
        Private rows As Integer, cols As Integer
        
        Public Sub New(r As Integer, c As Integer)
            rows = r
            cols = c
            ReDim data(r - 1, c - 1)
        End Sub
        
        Public Sub New(values(,) As Complex)
            rows = values.GetLength(0)
            cols = values.GetLength(1)
            data = values.Clone()
        End Sub
        
        Public Property Item(i As Integer, j As Integer) As Complex
            Get
                Return data(i, j)
            End Get
            Set(value As Complex)
                data(i, j) = value
            End Set
        End Property
        
        Public ReadOnly Property Rows As Integer
            Get
                Return rows
            End Get
        End Property
        
        Public ReadOnly Property Cols As Integer
            Get
                Return cols
            End Get
        End Property
        
        ' 矩阵加法
        Public Shared Operator +(m1 As ComplexMatrix, m2 As ComplexMatrix) As ComplexMatrix
            If m1.rows <> m2.rows OrElse m1.cols <> m2.cols Then
                Throw New ArgumentException("矩阵维度不匹配")
            End If
            
            Dim result As New ComplexMatrix(m1.rows, m1.cols)
            For i As Integer = 0 To m1.rows - 1
                For j As Integer = 0 To m1.cols - 1
                    result(i, j) = m1(i, j) + m2(i, j)
                Next
            Next
            Return result
        End Operator
        
        ' 矩阵乘法
        Public Shared Operator *(m1 As ComplexMatrix, m2 As ComplexMatrix) As ComplexMatrix
            If m1.cols <> m2.rows Then
                Throw New ArgumentException("矩阵维度不匹配")
            End If
            
            Dim result As New ComplexMatrix(m1.rows, m2.cols)
            For i As Integer = 0 To m1.rows - 1
                For j As Integer = 0 To m2.cols - 1
                    For k As Integer = 0 To m1.cols - 1
                        result(i, j) += m1(i, k) * m2(k, j)
                    Next
                Next
            Next
            Return result
        End Operator
        
        ' 标量乘法
        Public Shared Operator *(scalar As Complex, m As ComplexMatrix) As ComplexMatrix
            Dim result As New ComplexMatrix(m.rows, m.cols)
            For i As Integer = 0 To m.rows - 1
                For j As Integer = 0 To m.cols - 1
                    result(i, j) = scalar * m(i, j)
                Next
            Next
            Return result
        End Operator
        
        ' 矩阵的迹
        Public Function Trace() As Complex
            If rows <> cols Then
                Throw New InvalidOperationException("非方阵没有迹")
            End If
            
            Dim tr As Complex = Complex.Zero
            For i As Integer = 0 To rows - 1
                tr += data(i, i)
            Next
            Return tr
        End Function
        
        ' 共轭转置
        Public Function Adjoint() As ComplexMatrix
            Dim result As New ComplexMatrix(cols, rows)
            For i As Integer = 0 To rows - 1
                For j As Integer = 0 To cols - 1
                    result(j, i) = Complex.Conjugate(data(i, j))
                Next
            Next
            Return result
        End Function
        
        ' 对易子 [A, B] = AB - BA
        Public Shared Function Commutator(A As ComplexMatrix, B As ComplexMatrix) As ComplexMatrix
            Return A * B - B * A
        End Function
        
        ' 反对易子 {A, B} = AB + BA
        Public Shared Function AntiCommutator(A As ComplexMatrix, B As ComplexMatrix) As ComplexMatrix
            Return A * B + B * A
        End Function
        
        ' 转换为字符串（用于调试）
        Public Overrides Function ToString() As String
            Dim sb As New Text.StringBuilder()
            For i As Integer = 0 To Math.Min(rows - 1, 4)
                For j As Integer = 0 To Math.Min(cols - 1, 4)
                    sb.AppendFormat("{0:F2} ", data(i, j).Real)
                Next
                If cols > 5 Then sb.Append("...")
                sb.AppendLine()
            Next
            If rows > 5 Then sb.AppendLine("...")
            Return sb.ToString()
        End Function
    End Class
#End Region

#Region "量子态向量"
    Public Class QuantumStateVector
        Private components() As Complex
        Private dimension As Integer
        
        Public Sub New(dim As Integer)
            dimension = dim
            ReDim components(dim - 1)
            ' 初始化为基态
            components(0) = Complex.One
        End Sub
        
        Public Sub New(values() As Complex)
            components = values.Clone()
            dimension = values.Length
        End Sub
        
        Public Property Item(i As Integer) As Complex
            Get
                Return components(i)
            End Get
            Set(value As Complex)
                components(i) = value
            End Set
        End Property
        
        Public ReadOnly Property Dimension As Integer
            Get
                Return dimension
            End Get
        End Property
        
        ' 归一化
        Public Sub Normalize()
            Dim normSquared As Double = 0
            For i As Integer = 0 To dimension - 1
                normSquared += components(i).MagnitudeSquared
            Next
            
            Dim norm As Double = Math.Sqrt(normSquared)
            If norm > 0 Then
                For i As Integer = 0 To dimension - 1
                    components(i) /= norm
                Next
            End If
        End Sub
        
        ' 期望值
        Public Function ExpectationValue(operatorMatrix As ComplexMatrix) As Complex
            If operatorMatrix.Rows <> dimension OrElse operatorMatrix.Cols <> dimension Then
                Throw New ArgumentException("算符维度与态矢量不匹配")
            End If
            
            ' 计算 <ψ|A|ψ> = ψ† A ψ
            Dim result As Complex = Complex.Zero
            For i As Integer = 0 To dimension - 1
                For j As Integer = 0 To dimension - 1
                    result += Complex.Conjugate(components(i)) * operatorMatrix(i, j) * components(j)
                Next
            Next
            
            Return result
        End Function
        
        ' 测量概率
        Public Function MeasurementProbabilities(eigenvalues() As Double, 
                                                eigenvectors As List(Of QuantumStateVector)) As Double()
            Dim probs(eigenvalues.Length - 1) As Double
            
            For i As Integer = 0 To eigenvalues.Length - 1
                Dim eigenvector = eigenvectors(i)
                Dim probability As Double = 0
                
                ' 计算重叠 |<ψ|φ_i>|^2
                For j As Integer = 0 To dimension - 1
                    probability += Complex.Conjugate(components(j)) * eigenvector(j)
                Next
                
                probs(i) = probability.MagnitudeSquared
            Next
            
            Return probs
        End Function
    End Class
#End Region

#Region "海森堡量子系统"
    Public Class HeisenbergQuantumSystem
        Private dimension As Integer
        Private H As ComplexMatrix ' 哈密顿量
        Private X As ComplexMatrix ' 位置算符
        Private P As ComplexMatrix ' 动量算符
        
        Public Sub New(dim As Integer, potentialType As PotentialType)
            dimension = dim
            ConstructOperators(potentialType)
        End Sub
        
        ' 构造谐振子算符
        Private Sub ConstructOperators(potentialType As PotentialType)
            ' 构造位置和动量算符矩阵
            X = New ComplexMatrix(dimension, dimension)
            P = New ComplexMatrix(dimension, dimension)
            
            ' 离散化位置基
            Dim x_min As Double = -5.0
            Dim x_max As Double = 5.0
            Dim dx As Double = (x_max - x_min) / (dimension - 1)
            
            ' 位置算符（对角矩阵）
            For i As Integer = 0 To dimension - 1
                X(i, i) = x_min + i * dx
            Next
            
            ' 动量算符（有限差分近似）
            For i As Integer = 0 To dimension - 1
                For j As Integer = 0 To dimension - 1
                    If i = j + 1 Then
                        P(i, j) = -Complex.ImaginaryOne * hbar / (2 * dx)
                    ElseIf i + 1 = j Then
                        P(i, j) = Complex.ImaginaryOne * hbar / (2 * dx)
                    End If
                Next
            Next
            
            ' 构造哈密顿量 H = P^2/2m + V(X)
            Select Case potentialType
                Case PotentialType.HarmonicOscillator
                    H = ConstructHarmonicOscillatorHamiltonian()
                Case PotentialType.InfiniteWell
                    H = ConstructInfiniteWellHamiltonian()
                Case PotentialType.Coulomb
                    H = ConstructCoulombHamiltonian()
            End Select
        End Sub
        
        ' 谐振子哈密顿量
        Private Function ConstructHarmonicOscillatorHamiltonian() As ComplexMatrix
            ' H = P^2/2m + (1/2)mω^2 X^2
            Dim omega As Double = 1.0
            
            ' 计算 P^2
            Dim P2 As ComplexMatrix = P * P
            
            ' 计算 X^2
            Dim X2 As ComplexMatrix = X * X
            
            Return (1.0 / (2 * m)) * P2 + (0.5 * m * omega * omega) * X2
        End Function
        
        ' 无限深势阱哈密顿量
        Private Function ConstructInfiniteWellHamiltonian() As ComplexMatrix
            ' 在离散位置基中，边界条件自动满足
            Return (1.0 / (2 * m)) * (P * P)
        End Function
        
        ' 库仑势哈密顿量（简化版）
        Private Function ConstructCoulombHamiltonian() As ComplexMatrix
            Dim P2 As ComplexMatrix = P * P
            Dim V As New ComplexMatrix(dimension, dimension)
            
            For i As Integer = 0 To dimension - 1
                Dim x_value As Double = X(i, i).Real
                If Math.Abs(x_value) > 0.01 Then
                    V(i, i) = -1.0 / Math.Abs(x_value)
                Else
                    V(i, i) = -100.0 ' 避免奇点
                End If
            Next
            
            Return (1.0 / (2 * m)) * P2 + V
        End Function
        
        ' 求解本征值和本征态（简化的幂法）
        Public Function SolveEigenstates(numStates As Integer) As (eigenvalues() As Double, 
                                                                   eigenvectors As List(Of QuantumStateVector))
            Dim eigenvalues(numStates - 1) As Double
            Dim eigenvectors As New List(Of QuantumStateVector)()
            
            ' 复制哈密顿量
            Dim currentMatrix As ComplexMatrix = H
            
            For stateIndex As Integer = 0 To numStates - 1
                ' 使用幂法求主本征值
                Dim eigenvector = PowerMethod(currentMatrix, 1000)
                Dim eigenvalue As Complex = RayleighQuotient(currentMatrix, eigenvector)
                
                eigenvalues(stateIndex) = eigenvalue.Real
                eigenvectors.Add(eigenvector)
                
                ' 从矩阵中减去已找到的本征态
                currentMatrix = DeflateMatrix(currentMatrix, eigenvector, eigenvalue.Real)
            Next
            
            Return (eigenvalues, eigenvectors)
        End Function
        
        ' 幂法求主本征向量
        Private Function PowerMethod(matrix As ComplexMatrix, iterations As Integer) As QuantumStateVector
            Dim random As New Random()
            Dim v As New QuantumStateVector(dimension)
            
            ' 随机初始化
            For i As Integer = 0 To dimension - 1
                v(i) = New Complex(random.NextDouble() - 0.5, random.NextDouble() - 0.5)
            Next
            v.Normalize()
            
            For iter As Integer = 1 To iterations
                ' v = A * v
                Dim newV As New QuantumStateVector(dimension)
                For i As Integer = 0 To dimension - 1
                    For j As Integer = 0 To dimension - 1
                        newV(i) += matrix(i, j) * v(j)
                    Next
                Next
                
                v = newV
                v.Normalize()
                
                ' 每100次迭代检查收敛
                If iter Mod 100 = 0 Then
                    Dim lambda As Complex = RayleighQuotient(matrix, v)
                    ' 可以在这里添加收敛检查
                End If
            Next
            
            Return v
        End Function
        
        ' 瑞利商估计本征值
        Private Function RayleighQuotient(matrix As ComplexMatrix, 
                                         vector As QuantumStateVector) As Complex
            ' λ = <v|A|v> / <v|v>
            Dim numerator As Complex = Complex.Zero
            Dim denominator As Complex = Complex.Zero
            
            For i As Integer = 0 To dimension - 1
                Dim sum As Complex = Complex.Zero
                For j As Integer = 0 To dimension - 1
                    sum += matrix(i, j) * vector(j)
                Next
                numerator += Complex.Conjugate(vector(i)) * sum
                denominator += Complex.Conjugate(vector(i)) * vector(i)
            Next
            
            Return numerator / denominator
        End Function
        
        ' 矩阵放气：减去已找到的本征态
        Private Function DeflateMatrix(matrix As ComplexMatrix, 
                                      eigenvector As QuantumStateVector, 
                                      eigenvalue As Double) As ComplexMatrix
            ' A' = A - λ v v†
            Dim vvdagger As New ComplexMatrix(dimension, dimension)
            
            For i As Integer = 0 To dimension - 1
                For j As Integer = 0 To dimension - 1
                    vvdagger(i, j) = eigenvector(i) * Complex.Conjugate(eigenvector(j))
                Next
            Next
            
            Return matrix - eigenvalue * vvdagger
        End Function
        
        ' 海森堡运动方程：dA/dt = (i/ħ)[H, A]
        Public Function HeisenbergEvolution(operatorMatrix As ComplexMatrix, 
                                           time As Double) As ComplexMatrix
            Dim A_t As ComplexMatrix = operatorMatrix
            
            ' 使用矩阵指数：A(t) = e^(iHt/ħ) A e^(-iHt/ħ)
            Dim U As ComplexMatrix = MatrixExponential(H, time)
            Dim U_dagger As ComplexMatrix = U.Adjoint()
            
            Return U * A_t * U_dagger
        End Function
        
        ' 矩阵指数近似计算
        Private Function MatrixExponential(matrix As ComplexMatrix, 
                                          time As Double) As ComplexMatrix
            ' e^(iHt/ħ) ≈ I + iHt/ħ - (Ht/ħ)^2/2 + ...
            Dim I As ComplexMatrix = IdentityMatrix(dimension)
            Dim result As ComplexMatrix = I
            Dim term As ComplexMatrix = I
            
            Dim factor As Complex = Complex.ImaginaryOne * time / hbar
            
            ' 前10项近似
            For n As Integer = 1 To 10
                term = term * matrix * factor / n
                result = result + term
            Next
            
            Return result
        End Function
        
        ' 单位矩阵
        Private Function IdentityMatrix(size As Integer) As ComplexMatrix
            Dim I As New ComplexMatrix(size, size)
            For i As Integer = 0 To size - 1
                I(i, i) = Complex.One
            Next
            Return I
        End Function
        
        ' 验证对易关系
        Public Function VerifyCommutators() As Dictionary(Of String, Double)
            Dim results As New Dictionary(Of String, Double)()
            
            ' 验证基本对易关系 [X, P] = iħ
            Dim XP_commutator As ComplexMatrix = ComplexMatrix.Commutator(X, P)
            Dim expected As Complex = Complex.ImaginaryOne * hbar
            
            ' 计算对易子迹范数
            Dim diff As ComplexMatrix = XP_commutator - expected * IdentityMatrix(dimension)
            Dim error As Double = MatrixNorm(diff)
            
            results.Add("[X, P] = iħ", error)
            
            ' 验证海森堡方程
            Dim dX_dt As ComplexMatrix = (Complex.ImaginaryOne / hbar) * ComplexMatrix.Commutator(H, X)
            results.Add("Heisenberg dX/dt", MatrixNorm(dX_dt))
            
            Return results
        End Function
        
        ' 矩阵范数
        Private Function MatrixNorm(matrix As ComplexMatrix) As Double
            Dim sum As Double = 0
            For i As Integer = 0 To matrix.Rows - 1
                For j As Integer = 0 To matrix.Cols - 1
                    sum += matrix(i, j).MagnitudeSquared
                Next
            Next
            Return Math.Sqrt(sum / (matrix.Rows * matrix.Cols))
        End Function
        
        ' 不确定性关系：ΔX ΔP ≥ ħ/2
        Public Function CalculateUncertainty(state As QuantumStateVector) As (deltaX As Double, 
                                                                             deltaP As Double, 
                                                                             uncertaintyProduct As Double)
            Dim expX As Complex = state.ExpectationValue(X)
            Dim expP As Complex = state.ExpectationValue(P)
            
            ' 计算 X^2 和 P^2
            Dim X2 As ComplexMatrix = X * X
            Dim P2 As ComplexMatrix = P * P
            
            Dim expX2 As Complex = state.ExpectationValue(X2)
            Dim expP2 As Complex = state.ExpectationValue(P2)
            
            ' 方差
            Dim varX As Double = (expX2 - expX * expX).Real
            Dim varP As Double = (expP2 - expP * expP).Real
            
            Dim deltaX As Double = Math.Sqrt(Math.Max(0, varX))
            Dim deltaP As Double = Math.Sqrt(Math.Max(0, varP))
            Dim uncertaintyProduct As Double = deltaX * deltaP
            
            Return (deltaX, deltaP, uncertaintyProduct)
        End Function
    End Class
#End Region

#Region "枚举和辅助类"
    Public Enum PotentialType
        HarmonicOscillator
        InfiniteWell
        Coulomb
    End Enum
    
    Public Class QuantumMeasurementResult
        Public Property Eigenvalue As Double
        Public Property Probability As Double
        Public Property CollapsedState As QuantumStateVector
        
        Public Sub New(eVal As Double, prob As Double, state As QuantumStateVector)
            Eigenvalue = eVal
            Probability = prob
            CollapsedState = state
        End Sub
    End Class
#End Region
End Class

' ============================================
' 可视化界面
' ============================================
Public Class MatrixQuantumVisualizer
    Inherits Form
    
    Private WithEvents btnSolve As New Button()
    Private WithEvents btnAnimate As New Button()
    Private WithEvents cmbPotential As New ComboBox()
    Private WithEvents txtResults As New TextBox()
    Private WithEvents chartPanel As New Panel()
    Private WithEvents lblStatus As New Label()
    
    Private quantumSystem As MatrixQuantumMechanics.HeisenbergQuantumSystem
    Private currentStates As List(Of MatrixQuantumMechanics.QuantumStateVector)
    Private currentEigenvalues() As Double
    
    Public Sub New()
        InitializeComponent()
    End Sub
    
    Private Sub InitializeComponent()
        Me.Text = "海森堡矩阵量子力学模拟器"
        Me.Size = New Size(1000, 700)
        Me.BackColor = Color.FromArgb(30, 30, 40)
        
        ' 控制面板
        Dim controlPanel As New Panel()
        controlPanel.Location = New Point(20, 20)
        controlPanel.Size = New Size(960, 100)
        controlPanel.BackColor = Color.FromArgb(40, 40, 60)
        controlPanel.BorderStyle = BorderStyle.FixedSingle
        
        ' 势能选择
        Dim lblPotential As New Label()
        lblPotential.Text = "势能类型："
        lblPotential.ForeColor = Color.White
        lblPotential.Location = New Point(20, 20)
        
        cmbPotential.Items.AddRange({"谐振子", "无限深势阱", "库仑势"})
        cmbPotential.SelectedIndex = 0
        cmbPotential.Location = New Point(100, 20)
        cmbPotential.Size = New Size(150, 25)
        
        ' 求解按钮
        btnSolve.Text = "求解量子态"
        btnSolve.Location = New Point(270, 20)
        btnSolve.Size = New Size(120, 30)
        btnSolve.BackColor = Color.FromArgb(70, 130, 180)
        btnSolve.ForeColor = Color.White
        
        ' 动画按钮
        btnAnimate.Text = "时间演化"
        btnAnimate.Location = New Point(400, 20)
        btnAnimate.Size = New Size(120, 30)
        btnAnimate.BackColor = Color.FromArgb(70, 180, 130)
        btnAnimate.ForeColor = Color.White
        
        ' 状态标签
        lblStatus.Location = New Point(20, 60)
        lblStatus.Size = New Size(500, 25)
        lblStatus.ForeColor = Color.LightGreen
        
        controlPanel.Controls.AddRange({lblPotential, cmbPotential, 
                                       btnSolve, btnAnimate, lblStatus})
        
        ' 结果文本框
        txtResults.Location = New Point(20, 140)
        txtResults.Size = New Size(400, 500)
        txtResults.Multiline = True
        txtResults.ScrollBars = ScrollBars.Vertical
        txtResults.BackColor = Color.FromArgb(20, 20, 30)
        txtResults.ForeColor = Color.White
        txtResults.Font = New Font("Consolas", 10)
        
        ' 图表面板
        chartPanel.Location = New Point(440, 140)
        chartPanel.Size = New Size(540, 500)
        chartPanel.BackColor = Color.Black
        chartPanel.BorderStyle = BorderStyle.FixedSingle
        
        Me.Controls.AddRange({controlPanel, txtResults, chartPanel})
    End Sub
    
    Private Sub btnSolve_Click(sender As Object, e As EventArgs) Handles btnSolve.Click
        Try
            lblStatus.Text = "正在构造量子系统..."
            Application.DoEvents()
            
            ' 确定势能类型
            Dim potentialType As MatrixQuantumMechanics.PotentialType
            Select Case cmbPotential.SelectedItem.ToString()
                Case "无限深势阱"
                    potentialType = MatrixQuantumMechanics.PotentialType.InfiniteWell
                Case "库仑势"
                    potentialType = MatrixQuantumMechanics.PotentialType.Coulomb
                Case Else
                    potentialType = MatrixQuantumMechanics.PotentialType.HarmonicOscillator
            End Select
            
            ' 创建量子系统
            quantumSystem = New MatrixQuantumMechanics.HeisenbergQuantumSystem(50, potentialType)
            
            lblStatus.Text = "正在求解本征态..."
            Application.DoEvents()
            
            ' 求解本征态
            Dim result = quantumSystem.SolveEigenstates(5)
            currentEigenvalues = result.eigenvalues
            currentStates = result.eigenvectors
            
            ' 显示结果
            DisplayResults()
            
            ' 验证对易关系
            Dim commutatorResults = quantumSystem.VerifyCommutators()
            txtResults.AppendText(vbCrLf & "=== 对易关系验证 ===" & vbCrLf)
            For Each kvp In commutatorResults
                txtResults.AppendText($"{kvp.Key}: {kvp.Value:E4}" & vbCrLf)
            Next
            
            lblStatus.Text = "计算完成！"
            
            ' 绘图
            DrawResults()
            
        Catch ex As Exception
            MessageBox.Show($"错误: {ex.Message}", "计算错误", 
                          MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub
    
    Private Sub DisplayResults()
        txtResults.Clear()
        txtResults.AppendText("=== 海森堡矩阵量子力学结果 ===" & vbCrLf & vbCrLf)
        
        txtResults.AppendText("本征能量：" & vbCrLf)
        For i As Integer = 0 To currentEigenvalues.Length - 1
            txtResults.AppendText($"E{i + 1} = {currentEigenvalues(i):F4}" & vbCrLf)
        Next
        
        txtResults.AppendText(vbCrLf & "不确定性关系：" & vbCrLf)
        For i As Integer = 0 To currentStates.Count - 1
            Dim uncertainty = quantumSystem.CalculateUncertainty(currentStates(i))
            txtResults.AppendText($"态 {i + 1}: ΔX = {uncertainty.deltaX:F4}, " &
                                 $"ΔP = {uncertainty.deltaP:F4}, " &
                                 $"乘积 = {uncertainty.uncertaintyProduct:F4}" & vbCrLf)
            txtResults.AppendText($"   (ħ/2 = {0.5:F4})" & vbCrLf)
        Next
        
        txtResults.AppendText(vbCrLf & "期望值：" & vbCrLf)
        For i As Integer = 0 To currentStates.Count - 1
            Dim expX = currentStates(i).ExpectationValue(quantumSystem.GetType().GetField("X", 
                         Reflection.BindingFlags.NonPublic Or 
                         Reflection.BindingFlags.Instance).GetValue(quantumSystem))
            Dim expP = currentStates(i).ExpectationValue(quantumSystem.GetType().GetField("P", 
                         Reflection.BindingFlags.NonPublic Or 
                         Reflection.BindingFlags.Instance).GetValue(quantumSystem))
            txtResults.AppendText($"态 {i + 1}: <X> = {expX.Real:F4}, <P> = {expP.Real:F4}" & vbCrLf)
        Next
    End Sub
    
    Private Sub DrawResults()
        Dim g As Graphics = chartPanel.CreateGraphics()
        g.Clear(Color.Black)
        
        ' 绘制坐标轴
        Dim penAxis As New Pen(Color.White, 2)
        Dim centerX As Integer = chartPanel.Width \ 2
        Dim centerY As Integer = chartPanel.Height \ 2
        
        g.DrawLine(penAxis, 50, centerY, chartPanel.Width - 50, centerY) ' X轴
        g.DrawLine(penAxis, centerX, 50, centerX, chartPanel.Height - 50) ' Y轴
        
        ' 绘制能量水平线
        Dim penEnergy As New Pen(Color.Green, 1)
        Dim maxEnergy As Double = If(currentEigenvalues.Length > 0, 
                                   currentEigenvalues.Max(), 10)
        
        For i As Integer = 0 To currentEigenvalues.Length - 1
            Dim y As Integer = centerY - CInt(currentEigenvalues(i) / maxEnergy * 200)
            g.DrawLine(penEnergy, 50, y, chartPanel.Width - 50, y)
            
            ' 标记能量值
            Dim brush As New SolidBrush(Color.Yellow)
            g.DrawString($"E{i + 1} = {currentEigenvalues(i):F2}", 
                        New Font("Arial", 10), brush, chartPanel.Width - 150, y - 15)
        Next
        
        ' 绘制波函数概率分布
        Dim colors() As Color = {Color.Red, Color.Cyan, Color.Magenta, Color.Yellow, Color.Orange}
        
        For stateIndex As Integer = 0 To Math.Min(4, currentStates.Count - 1)
            Dim state = currentStates(stateIndex)
            Dim points(state.Dimension - 1) As Point
            
            For i As Integer = 0 To state.Dimension - 1
                Dim x As Integer = 50 + i * (chartPanel.Width - 100) \ state.Dimension
                Dim probability As Double = state(i).MagnitudeSquared
                Dim y As Integer = centerY - CInt(probability * 100 * (stateIndex + 1))
                points(i) = New Point(x, y)
            Next
            
            Dim penWave As New Pen(colors(stateIndex), 2)
            g.DrawLines(penWave, points)
        Next
        
        ' 绘制标题
        Dim titleBrush As New SolidBrush(Color.White)
        g.DrawString("海森堡矩阵量子力学 - 能量本征态", 
                    New Font("Arial", 14, FontStyle.Bold), 
                    titleBrush, 150, 10)
        
        g.DrawString("X方向：位置基，Y方向：概率密度（缩放）", 
                    New Font("Arial", 10), 
                    titleBrush, 150, chartPanel.Height - 30)
                    
        ' 清理资源
        penAxis.Dispose()
        penEnergy.Dispose()
        For Each pen In colors
            Dim p As New Pen(pen, 2)
            p.Dispose()
        Next
        titleBrush.Dispose()
    End Sub
    
    Private Sub btnAnimate_Click(sender As Object, e As EventArgs) Handles btnAnimate.Click
        If quantumSystem Is Nothing Then
            MessageBox.Show("请先求解量子系统！")
            Return
        End If
        
        ' 演示时间演化
        lblStatus.Text = "正在进行时间演化..."
        
        Dim timer As New Timer()
        timer.Interval = 100 ' 100ms
        Dim time As Double = 0
        
        AddHandler timer.Tick, Sub(s, ev)
                                  time += 0.1
                                  
                                  ' 计算时间演化后的期望值
                                  Dim g As Graphics = chartPanel.CreateGraphics()
                                  g.Clear(Color.Black)
                                  
                                  ' 这里可以添加时间演化的可视化代码
                                  ' 例如，显示随时间变化的期望位置
                                  
                                  If time > 10 Then
                                      timer.Stop()
                                      timer.Dispose()
                                      lblStatus.Text = "时间演化完成"
                                      DrawResults()
                                  End If
                              End Sub
        
        timer.Start()
    End Sub
    
    Protected Overrides Sub OnPaint(e As PaintEventArgs)
        MyBase.OnPaint(e)
        DrawResults()
    End Sub
End Class

' ============================================
' 使用示例
' ============================================
Module MainModule
    <STAThread>
    Sub Main()
        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault(False)
        Application.Run(New MatrixQuantumVisualizer())
    End Sub
    
    ' 命令行测试
    Sub TestHeisenbergMechanics()
        Console.WriteLine("=== 海森堡矩阵量子力学测试 ===")
        
        ' 创建谐振子系统
        Dim qm As New MatrixQuantumMechanics()
        Dim system = New MatrixQuantumMechanics.HeisenbergQuantumSystem(
            20, MatrixQuantumMechanics.PotentialType.HarmonicOscillator)
        
        ' 求解本征态
        Dim eigenstates = system.SolveEigenstates(3)
        Console.WriteLine(vbCrLf & "前3个本征能量：")
        For i As Integer = 0 To eigenstates.eigenvalues.Length - 1
            Console.WriteLine($"E{i + 1} = {eigenstates.eigenvalues(i):F4}")
        Next
        
        ' 计算基态的不确定性
        Dim groundState = eigenstates.eigenvectors(0)
        Dim uncertainty = system.CalculateUncertainty(groundState)
        Console.WriteLine(vbCrLf & "基态不确定性：")
        Console.WriteLine($"ΔX = {uncertainty.deltaX:F4}")
        Console.WriteLine($"ΔP = {uncertainty.deltaP:F4}")
        Console.WriteLine($"ΔX ΔP = {uncertainty.uncertaintyProduct:F4}")
        Console.WriteLine($"ħ/2 = {0.5:F4}")
        
        ' 验证对易关系
        Dim commutators = system.VerifyCommutators()
        Console.WriteLine(vbCrLf & "对易关系验证：")
        For Each kvp In commutators
            Console.WriteLine($"{kvp.Key}: {kvp.Value:E4}")
        Next
        Console.WriteLine(vbCrLf & "测试完成！")
    End Sub
End Module