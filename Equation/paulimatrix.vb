Public Class SpinSystem
    ' 泡利矩阵
    Public Shared ReadOnly Property SigmaX As MatrixQuantumMechanics.ComplexMatrix
        Get
            Return New MatrixQuantumMechanics.ComplexMatrix(
                {{Complex.Zero, Complex.One},
                 {Complex.One, Complex.Zero}})
        End Get
    End Property
    
    Public Shared ReadOnly Property SigmaY As MatrixQuantumMechanics.ComplexMatrix
        Get
            Return New MatrixQuantumMechanics.ComplexMatrix(
                {{Complex.Zero, -Complex.ImaginaryOne},
                 {Complex.ImaginaryOne, Complex.Zero}})
        End Get
    End Property
    
    Public Shared ReadOnly Property SigmaZ As MatrixQuantumMechanics.ComplexMatrix
        Get
            Return New MatrixQuantumMechanics.ComplexMatrix(
                {{Complex.One, Complex.Zero},
                 {Complex.Zero, -Complex.One}})
        End Get
    End Property
    
    ' 自旋1/2系统的哈密顿量（在磁场中）
    Public Shared Function SpinHamiltonian(Bx As Double, By As Double, Bz As Double) 
        As MatrixQuantumMechanics.ComplexMatrix
        ' H = -μ·B = -γ(S·B) = -(γħ/2)(σ·B)
        ' 简化：取 γħ/2 = 1
        Return -(Bx * SigmaX + By * SigmaY + Bz * SigmaZ)
    End Function
    
    ' 自旋向上和向下态
    Public Shared ReadOnly Property SpinUp As MatrixQuantumMechanics.QuantumStateVector
        Get
            Return New MatrixQuantumMechanics.QuantumStateVector(
                {Complex.One, Complex.Zero})
        End Get
    End Property
    
    Public Shared ReadOnly Property SpinDown As MatrixQuantumMechanics.QuantumStateVector
        Get
            Return New MatrixQuantumMechanics.QuantumStateVector(
                {Complex.Zero, Complex.One})
        End Get
    End Property
    
    ' 计算自旋期望值
    Public Shared Function ExpectationSpin(state As MatrixQuantumMechanics.QuantumStateVector) 
        As (Sx As Double, Sy As Double, Sz As Double)
        Dim Sx = state.ExpectationValue(SigmaX).Real
        Dim Sy = state.ExpectationValue(SigmaY).Real
        Dim Sz = state.ExpectationValue(SigmaZ).Real
        Return (Sx, Sy, Sz)
    End Function
    
    ' 拉莫尔进动
    Public Shared Function LarmorPrecession(initialState As MatrixQuantumMechanics.QuantumStateVector,
                                            B As (x As Double, y As Double, z As Double),
                                            time As Double) As MatrixQuantumMechanics.QuantumStateVector
        Dim H = SpinHamiltonian(B.x, B.y, B.z)
        
        ' 时间演化算符 U = exp(-iHt/ħ)
        Dim U = MatrixExponential(H, time)
        
        ' 演化态：|ψ(t)> = U|ψ(0)>
        Dim evolvedState = U * initialState
        evolvedState.Normalize()
        
        Return evolvedState
    End Function
    
    Private Shared Function MatrixExponential(A As MatrixQuantumMechanics.ComplexMatrix,
                                             t As Double) As MatrixQuantumMechanics.ComplexMatrix
        ' 对于2x2矩阵，可以解析计算
        ' e^(-iHt) = cos(ωt)I - i sin(ωt)(H/ω)
        Dim trace = A.Trace()
        Dim det = A(0,0)*A(1,1) - A(0,1)*A(1,0)
        
        Dim omega = Math.Sqrt(-det.Real) ' 假设H是厄米的
        
        Dim I = New MatrixQuantumMechanics.ComplexMatrix(
            {{Complex.One, Complex.Zero},
             {Complex.Zero, Complex.One}})
        
        Dim cos_term = Math.Cos(omega * t) * I
        Dim sin_term = (Complex.ImaginaryOne * Math.Sin(omega * t) / omega) * A
        
        Return cos_term - sin_term
    End Function
    
    Private Shared Operator *(U As MatrixQuantumMechanics.ComplexMatrix,
                              state As MatrixQuantumMechanics.QuantumStateVector) 
        As MatrixQuantumMechanics.QuantumStateVector
        If U.Cols <> state.Dimension Then
            Throw New ArgumentException("维度不匹配")
        End If
        
        Dim result(U.Rows - 1) As Complex
        For i As Integer = 0 To U.Rows - 1
            For j As Integer = 0 To U.Cols - 1
                result(i) += U(i, j) * state(j)
            Next
        Next
        
        Return New MatrixQuantumMechanics.QuantumStateVector(result)
    End Operator
End Class