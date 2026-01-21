Imports System.Windows.Forms
Imports System.Windows.Forms.DataVisualization.Charting

Public Class WaveFunctionVisualizer
    Inherits Form
    
    Private chart As New Chart()
    Private WithEvents btnCalculate As New Button()
    
    Public Sub New()
        InitializeComponent()
    End Sub
    
    Private Sub InitializeComponent()
        Me.Text = "波函数可视化"
        Me.Size = New Size(800, 600)
        
        ' 设置图表
        chart.Size = New Size(750, 500)
        chart.Location = New Point(25, 50)
        
        Dim chartArea As New ChartArea()
        chartArea.AxisX.Title = "位置 x"
        chartArea.AxisY.Title = "|Ψ|²"
        chart.ChartAreas.Add(chartArea)
        
        ' 添加按钮
        btnCalculate.Text = "计算并绘图"
        btnCalculate.Location = New Point(350, 10)
        btnCalculate.Size = New Size(100, 30)
        
        Me.Controls.Add(chart)
        Me.Controls.Add(btnCalculate)
    End Sub
    
    Private Sub btnCalculate_Click(sender As Object, e As EventArgs) Handles btnCalculate.Click
        Dim series As New Series("概率密度")
        series.ChartType = SeriesChartType.Line
        
        Dim waveNorm As New WaveFunctionNormalization()
        Dim normConst As Double = waveNorm.MonteCarloNormalization(100000).NormalizationConstant
        
        ' 生成一维截面图
        For x As Double = -3 To 3 Step 0.1
            Dim psi As Complex = waveNorm.NormalizedWaveFunction(x, 0, 0, 0, normConst)
            series.Points.AddXY(x, psi.MagnitudeSquared)
        Next
        
        chart.Series.Clear()
        chart.Series.Add(series)
    End Sub
End Class