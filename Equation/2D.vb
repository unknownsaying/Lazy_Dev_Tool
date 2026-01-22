Imports System.Math
Public Class TwoDSchrodingerSolver
    Private dx, dy As Double
    Private x_min, x_max, y_min, y_max As Double
    Private Nx, Ny As Integer
    Private potential As Func(Of Double, Double, Double)
    
    Public Sub New(xmin As Double, xmax As Double, ymin As Double, ymax As Double,
                   nx As Integer, ny As Integer, potentialFunc As Func(Of Double, Double, Double))
        x_min = xmin : x_max = xmax
        y_min = ymin : y_max = ymax
        Nx = nx : Ny = ny
        dx = (x_max - x_min) / (Nx - 1)
        dy = (y_max - y_min) / (Ny - 1)
        potential = potentialFunc
    End Sub
    
    ' 二维谐振子
    Public Function Solve2DHarmonicOscillator() As Complex(,)
        Dim psi(Nx - 1, Ny - 1) As Complex
        
        ' 基态波函数：高斯波包
        For i As Integer = 0 To Nx - 1
            For j As Integer = 0 To Ny - 1
                Dim x As Double = x_min + i * dx
                Dim y As Double = y_min + j * dy
                
                psi(i, j) = New Complex(
                    Exp(-(x * x + y * y) / 2),
                    0
                )
            Next
        Next
        
        Normalize2D(psi)
        Return psi
    End Function
    
    Private Sub Normalize2D(psi As Complex(,))
        Dim normSquared As Double = 0
        
        For i As Integer = 0 To Nx - 1
            For j As Integer = 0 To Ny - 1
                normSquared += psi(i, j).MagnitudeSquared() * dx * dy
            Next
        Next
        
        Dim norm As Double = Sqrt(normSquared)
        
        If norm > 0 Then
            For i As Integer = 0 To Nx - 1
                For j As Integer = 0 To Ny - 1
                    psi(i, j) = psi(i, j) / norm
                Next
            Next
        End If
    End Sub
End Class

' ============================================
' 氢原子波函数（解析解）
' ============================================
Public Class HydrogenWaveFunctions
    ' 球谐函数 Y_lm(theta, phi)
    Public Shared Function SphericalHarmonic(l As Integer, m As Integer, 
                                            theta As Double, phi As Double) As Complex
        Select Case l
            Case 0 ' s轨道
                Return New Complex(1 / Sqrt(4 * PI), 0)
                
            Case 1 ' p轨道
                Select Case m
                    Case 0
                        Return New Complex(Sqrt(3 / (4 * PI)) * Cos(theta), 0)
                    Case 1
                        Dim value As Double = -Sqrt(3 / (8 * PI)) * Sin(theta) * 
                                             Cos(phi)
                        Return New Complex(value, 0)
                    Case -1
                        Dim value As Double = Sqrt(3 / (8 * PI)) * Sin(theta) * 
                                             Sin(phi)
                        Return New Complex(value, 0)
                End Select
        End Select
        
        Return Complex.Zero
    End Function
    
    ' 径向波函数 R_nl(r)
    Public Shared Function RadialWaveFunction(n As Integer, l As Integer, r As Double) As Double
        Dim a0 As Double = PhysicalConstants.BohrRadius
        
        Select Case n
            Case 1 ' 1s轨道
                Return 2 * Exp(-r / a0) / Sqrt(a0 ^ 3)
                
            Case 2
                Select Case l
                    Case 0 ' 2s轨道
                        Return (1 / Sqrt(8 * a0 ^ 3)) * 
                               (2 - r / a0) * Exp(-r / (2 * a0))
                    Case 1 ' 2p轨道
                        Return (1 / Sqrt(24 * a0 ^ 3)) * 
                               (r / a0) * Exp(-r / (2 * a0))
                End Select
        End Select
        
        Return 0
    End Function
    
    ' 完整的氢原子波函数 Psi_nlm(r, theta, phi)
    Public Shared Function HydrogenAtomWaveFunction(n As Integer, l As Integer, m As Integer,
                                                   r As Double, theta As Double, phi As Double) As Complex
        Dim radial As Double = RadialWaveFunction(n, l, r)
        Dim angular As Complex = SphericalHarmonic(l, m, theta, phi)
        
        Return radial * angular
    End Function

End Class
