Imports System
Imports System.Numerics
Imports System.Windows.Forms
Imports System.Drawing
Imports System.Collections.Generic

' ============================================
' 流形核心类
' ============================================
Public Class Manifold3D
    
#Region "流形定义和结构"
    ' 流形点
    Public Class ManifoldPoint
        Public Property Coordinates As Vector3
        Public Property TangentSpace As Matrix4x4 ' 切空间基
        Public Property Normal As Vector3 ' 法向量
        
        Public Sub New(x As Single, y As Single, z As Single)
            Coordinates = New Vector3(x, y, z)
            TangentSpace = Matrix4x4.Identity
            Normal = Vector3.UnitZ
        End Sub
        
        Public Sub New(pos As Vector3)
            Coordinates = pos
            TangentSpace = Matrix4x4.Identity
            Normal = Vector3.UnitZ
        End Sub
        
        ' 变换到另一个坐标系
        Public Function Transform(transformMatrix As Matrix4x4) As ManifoldPoint
            Dim transformed = Vector3.Transform(Coordinates, transformMatrix)
            Return New ManifoldPoint(transformed)
        End Function
        
        Public Overrides Function ToString() As String
            Return $"({Coordinates.X:F2}, {Coordinates.Y:F2}, {Coordinates.Z:F2})"
        End Function
    End Class
    
    ' 流形面
    Public Class ManifoldFace
        Public Property Vertices As ManifoldPoint()
        Public Property Centroid As ManifoldPoint
        Public Property Area As Single
        
        Public Sub New(v1 As ManifoldPoint, v2 As ManifoldPoint, v3 As ManifoldPoint)
            Vertices = {v1, v2, v3}
            CalculateProperties()
        End Sub
        
        Private Sub CalculateProperties()
            ' 计算质心
            Dim sumX As Single = 0, sumY As Single = 0, sumZ As Single = 0
            For Each v In Vertices
                sumX += v.Coordinates.X
                sumY += v.Coordinates.Y
                sumZ += v.Coordinates.Z
            Next
            Centroid = New ManifoldPoint(sumX / 3, sumY / 3, sumZ / 3)
            
            ' 计算面积
            Dim v0 = Vertices(0).Coordinates
            Dim v1 = Vertices(1).Coordinates
            Dim v2 = Vertices(2).Coordinates
            
            Dim a = v1 - v0
            Dim b = v2 - v0
            Dim cross = Vector3.Cross(a, b)
            Area = cross.Length() / 2
        End Sub
        
        ' 获取法向量
        Public Function GetNormal() As Vector3
            Dim v0 = Vertices(0).Coordinates
            Dim v1 = Vertices(1).Coordinates
            Dim v2 = Vertices(2).Coordinates
            
            Dim a = v1 - v0
            Dim b = v2 - v0
            Dim normal = Vector3.Cross(a, b)
            normal = Vector3.Normalize(normal)
            Return normal
        End Function
    End Class
    
    ' 流形结构
    Public Class ManifoldStructure
        Public Property Points As List(Of ManifoldPoint)
        Public Property Faces As List(Of ManifoldFace)
        Public Property Edges As List(Of (Integer, Integer))
        
        Public Sub New()
            Points = New List(Of ManifoldPoint)()
            Faces = New List(Of ManifoldFace)()
            Edges = New List(Of (Integer, Integer))()
        End Sub
        
        ' 计算边界框
        Public Function GetBoundingBox() As (Min As Vector3, Max As Vector3)
            If Points.Count = 0 Then
                Return (Vector3.Zero, Vector3.Zero)
            End If
            
            Dim min = Points(0).Coordinates
            Dim max = Points(0).Coordinates
            
            For Each point In Points
                min = Vector3.Min(min, point.Coordinates)
                max = Vector3.Max(max, point.Coordinates)
            Next
            
            Return (min, max)
        End Function
        
        ' 计算体积（近似）
        Public Function CalculateVolume() As Single
            Dim volume As Single = 0
            
            For Each face In Faces
                ' 使用质心和原点的四面体
                Dim v0 = face.Vertices(0).Coordinates
                Dim v1 = face.Vertices(1).Coordinates
                Dim v2 = face.Vertices(2).Coordinates
                
                ' 四面体体积公式：V = |(a·(b×c))| / 6
                Dim tripleProduct = Vector3.Dot(v0, Vector3.Cross(v1, v2))
                volume += Math.Abs(tripleProduct) / 6
            Next
            
            Return volume
        End Function
    End Class
#End Region

#Region "流形构建器"
    Public Class ManifoldBuilder
        Private Const PI As Single = Math.PI
        
        ' 方法1：构建六方向流形（从-3,-6,-9到3,6,9）
        Public Function BuildSixDirectionManifold(numSegments As Integer) As ManifoldStructure
            Dim structure As New ManifoldStructure()
            
            ' 创建6个方向的面：±X, ±Y, ±Z
            Dim directions As Vector3() = {
                Vector3.UnitX, -Vector3.UnitX,
                Vector3.UnitY, -Vector3.UnitY,
                Vector3.UnitZ, -Vector3.UnitZ
            }
            
            Dim scales As Vector3() = {
                New Vector3(3, 6, 9),   ' 第一组坐标系
                New Vector3(2, 4, 8)    ' 第二组坐标系
            }
            
            ' 对每个方向创建面
            For dirIndex As Integer = 0 To directions.Length - 1
                Dim direction = directions(dirIndex)
                
                ' 为两个坐标系创建面
                For scaleIndex As Integer = 0 To scales.Length - 1
                    Dim scale = scales(scaleIndex)
                    
                    ' 创建该方向的面
                    Dim facePoints = CreateDirectionFace(direction, scale, numSegments)
                    structure.Points.AddRange(facePoints)
                    
                    ' 创建三角形面
                    Dim baseIndex = structure.Points.Count - facePoints.Count
                    CreateFaceTriangles(structure, baseIndex, numSegments)
                Next
            Next
            
            ' 创建连接边
            CreateEdges(structure)
            
            Return structure
        End Function
        
        ' 创建方向面
        Private Function CreateDirectionFace(direction As Vector3, scale As Vector3, 
                                            numSegments As Integer) As List(Of ManifoldPoint)
            Dim points As New List(Of ManifoldPoint)()
            
            ' 根据方向确定基面
            Dim uAxis, vAxis As Vector3
            
            If direction = Vector3.UnitX OrElse direction = -Vector3.UnitX Then
                uAxis = Vector3.UnitY
                vAxis = Vector3.UnitZ
            ElseIf direction = Vector3.UnitY OrElse direction = -Vector3.UnitY Then
                uAxis = Vector3.UnitX
                vAxis = Vector3.UnitZ
            Else ' ±Z
                uAxis = Vector3.UnitX
                vAxis = Vector3.UnitY
            End If
            
            ' 创建网格点
            For i As Integer = 0 To numSegments
                For j As Integer = 0 To numSegments
                    ' 参数化坐标
                    Dim u = -scale.Y + 2 * scale.Y * i / numSegments
                    Dim v = -scale.Z + 2 * scale.Z * j / numSegments
                    
                    ' 计算位置
                    Dim pos As Vector3 = direction * scale.X + u * uAxis + v * vAxis
                    Dim point As New ManifoldPoint(pos)
                    
                    ' 计算切空间（简化）
                    point.TangentSpace = CreateTangentSpace(direction, uAxis, vAxis)
                    point.Normal = direction
                    
                    points.Add(point)
                Next
            Next
            
            Return points
        End Function
        
        ' 创建切空间矩阵
        Private Function CreateTangentSpace(normal As Vector3, uAxis As Vector3, 
                                           vAxis As Vector3) As Matrix4x4
            ' 正交化
            Dim tangent = Vector3.Normalize(uAxis)
            Dim bitangent = Vector3.Normalize(vAxis)
            
            ' 确保正交
            bitangent = Vector3.Normalize(Vector3.Cross(normal, tangent))
            tangent = Vector3.Normalize(Vector3.Cross(bitangent, normal))
            
            Return New Matrix4x4(
                tangent.X, tangent.Y, tangent.Z, 0,
                bitangent.X, bitangent.Y, bitangent.Z, 0,
                normal.X, normal.Y, normal.Z, 0,
                0, 0, 0, 1)
        End Function
        
        ' 创建三角形面
        Private Sub CreateFaceTriangles(structure As ManifoldStructure, 
                                       baseIndex As Integer, numSegments As Integer)
            For i As Integer = 0 To numSegments - 1
                For j As Integer = 0 To numSegments - 1
                    ' 计算网格索引
                    Dim idx0 = baseIndex + i * (numSegments + 1) + j
                    Dim idx1 = idx0 + 1
                    Dim idx2 = baseIndex + (i + 1) * (numSegments + 1) + j
                    Dim idx3 = idx2 + 1
                    
                    ' 创建两个三角形
                    Dim tri1 As New ManifoldFace(
                        structure.Points(idx0),
                        structure.Points(idx1),
                        structure.Points(idx2))
                    
                    Dim tri2 As New ManifoldFace(
                        structure.Points(idx1),
                        structure.Points(idx3),
                        structure.Points(idx2))
                    
                    structure.Faces.Add(tri1)
                    structure.Faces.Add(tri2)
                Next
            Next
        End Sub
        
        ' 创建边
        Private Sub CreateEdges(structure As ManifoldStructure)
            For Each face In structure.Faces
                For i As Integer = 0 To 2
                    Dim v1 = structure.Points.IndexOf(face.Vertices(i))
                    Dim v2 = structure.Points.IndexOf(face.Vertices((i + 1) Mod 3))
                    
                    ' 添加边（确保唯一性）
                    Dim edge = (Math.Min(v1, v2), Math.Max(v1, v2))
                    If Not structure.Edges.Contains(edge) Then
                        structure.Edges.Add(edge)
                    End If
                Next
            Next
        End Sub
        
        ' 方法2：构建球形流形（在两个坐标系间插值）
        Public Function BuildSphericalManifold(radius1 As Single, radius2 As Single, 
                                              numMeridians As Integer, 
                                              numParallels As Integer) As ManifoldStructure
            Dim structure As New ManifoldStructure()
            
            ' 创建球面点
            For parallel As Integer = 0 To numParallels
                Dim theta = PI * parallel / numParallels ' 纬度角 [0, π]
                
                For meridian As Integer = 0 To numMeridians - 1
                    Dim phi = 2 * PI * meridian / numMeridians ' 经度角 [0, 2π]
                    
                    ' 球坐标转直角坐标
                    Dim x = Math.Sin(theta) * Math.Cos(phi)
                    Dim y = Math.Sin(theta) * Math.Sin(phi)
                    Dim z = Math.Cos(theta)
                    
                    ' 在两个半径间插值
                    Dim t As Single = parallel / numParallels
                    Dim radius = radius1 + (radius2 - radius1) * t
                    
                    Dim pos = New Vector3(x * radius, y * radius, z * radius)
                    Dim point As New ManifoldPoint(pos)
                    
                    ' 计算法向量（指向球心）
                    point.Normal = Vector3.Normalize(pos)
                    
                    structure.Points.Add(point)
                Next
            Next
            
            ' 创建三角形面
            For parallel As Integer = 0 To numParallels - 1
                For meridian As Integer = 0 To numMeridians - 1
                    Dim idx0 = parallel * numMeridians + meridian
                    Dim idx1 = parallel * numMeridians + (meridian + 1) Mod numMeridians
                    Dim idx2 = (parallel + 1) * numMeridians + meridian
                    Dim idx3 = (parallel + 1) * numMeridians + (meridian + 1) Mod numMeridians
                    
                    ' 创建两个三角形
                    Dim tri1 As New ManifoldFace(
                        structure.Points(idx0),
                        structure.Points(idx1),
                        structure.Points(idx2))
                    
                    Dim tri2 As New ManifoldFace(
                        structure.Points(idx1),
                        structure.Points(idx3),
                        structure.Points(idx2))
                    
                    structure.Faces.Add(tri1)
                    structure.Faces.Add(tri2)
                Next
            Next
            
            Return structure
        End Function
        
        ' 方法3：构建环面流形
        Public Function BuildTorusManifold(majorRadius As Single, minorRadius As Single,
                                          numMajorSegments As Integer, 
                                          numMinorSegments As Integer) As ManifoldStructure
            Dim structure As New ManifoldStructure()
            
            For i As Integer = 0 To numMajorSegments - 1
                Dim majorAngle = 2 * PI * i / numMajorSegments
                
                Dim cosMajor = Math.Cos(majorAngle)
                Dim sinMajor = Math.Sin(majorAngle)
                
                For j As Integer = 0 To numMinorSegments - 1
                    Dim minorAngle = 2 * PI * j / numMinorSegments
                    
                    Dim cosMinor = Math.Cos(minorAngle)
                    Dim sinMinor = Math.Sin(minorAngle)
                    
                    ' 环面参数方程
                    Dim x = (majorRadius + minorRadius * cosMinor) * cosMajor
                    Dim y = (majorRadius + minorRadius * cosMinor) * sinMajor
                    Dim z = minorRadius * sinMinor
                    
                    Dim pos = New Vector3(x, y, z)
                    Dim point As New ManifoldPoint(pos)
                    
                    ' 计算法向量
                    Dim normal = New Vector3(cosMajor * cosMinor, sinMajor * cosMinor, sinMinor)
                    point.Normal = Vector3.Normalize(normal)
                    
                    structure.Points.Add(point)
                Next
            Next
            
            ' 创建四边形面（分解为三角形）
            For i As Integer = 0 To numMajorSegments - 1
                For j As Integer = 0 To numMinorSegments - 1
                    Dim nextI = (i + 1) Mod numMajorSegments
                    Dim nextJ = (j + 1) Mod numMinorSegments
                    
                    Dim idx00 = i * numMinorSegments + j
                    Dim idx01 = i * numMinorSegments + nextJ
                    Dim idx10 = nextI * numMinorSegments + j
                    Dim idx11 = nextI * numMinorSegments + nextJ
                    
                    ' 创建两个三角形
                    Dim tri1 As New ManifoldFace(
                        structure.Points(idx00),
                        structure.Points(idx01),
                        structure.Points(idx10))
                    
                    Dim tri2 As New ManifoldFace(
                        structure.Points(idx01),
                        structure.Points(idx11),
                        structure.Points(idx10))
                    
                    structure.Faces.Add(tri1)
                    structure.Faces.Add(tri2)
                Next
            Next
            
            Return structure
        End Function
        
        ' 方法4：构建双坐标系混合流形
        Public Function BuildDualCoordinateManifold() As ManifoldStructure
            Dim structure As New ManifoldStructure()
            
            ' 坐标系1：(-3,-6,-9) 到 (3,6,9)
            Dim coord1Points = CreateCoordinateBox(-3, 3, -6, 6, -9, 9, 2)
            
            ' 坐标系2：(-2,-4,-8) 到 (2,4,8)
            Dim coord2Points = CreateCoordinateBox(-2, 2, -4, 4, -8, 8, 2)
            
            ' 合并点
            structure.Points.AddRange(coord1Points)
            structure.Points.AddRange(coord2Points)
            
            ' 创建连接面（在两个坐标系之间）
            CreateConnectionFaces(structure, 0, coord1Points.Count, coord2Points.Count)
            
            Return structure
        End Function
        
        ' 创建坐标框
        Private Function CreateCoordinateBox(xMin As Single, xMax As Single,
                                            yMin As Single, yMax As Single,
                                            zMin As Single, zMax As Single,
                                            segments As Integer) As List(Of ManifoldPoint)
            Dim points As New List(Of ManifoldPoint)()
            
            ' 创建8个角点
            Dim corners As Vector3() = {
                New Vector3(xMin, yMin, zMin),
                New Vector3(xMax, yMin, zMin),
                New Vector3(xMin, yMax, zMin),
                New Vector3(xMax, yMax, zMin),
                New Vector3(xMin, yMin, zMax),
                New Vector3(xMax, yMin, zMax),
                New Vector3(xMin, yMax, zMax),
                New Vector3(xMax, yMax, zMax)
            }
            
            For Each corner In corners
                points.Add(New ManifoldPoint(corner))
            Next
            
            ' 在边上添加额外点
            If segments > 1 Then
                For i As Integer = 1 To segments - 1
                    Dim t = i / segments
                    
                    ' 12条边
                    Dim edges = {
                        (0, 1), (1, 3), (3, 2), (2, 0), ' 底面
                        (4, 5), (5, 7), (7, 6), (6, 4), ' 顶面
                        (0, 4), (1, 5), (2, 6), (3, 7)  ' 垂直边
                    }
                    
                    For Each edge In edges
                        Dim p1 = corners(edge.Item1)
                        Dim p2 = corners(edge.Item2)
                        Dim interp = p1 + (p2 - p1) * t
                        points.Add(New ManifoldPoint(interp))
                    Next
                Next
            End If
            
            Return points
        End Function
        
        ' 创建连接面
        Private Sub CreateConnectionFaces(structure As ManifoldStructure,
                                         startIdx1 As Integer, count1 As Integer,
                                         startIdx2 As Integer)
            ' 在第一个坐标系的点和第二个坐标系的对应点之间创建面
            ' 这里简化处理：创建一些三角形连接两个框
            
            For i As Integer = 0 To Math.Min(count1 - 1, 7)
                For j As Integer = 0 To 7
                    ' 创建三角形连接两个坐标系的对应角点
                    If i <> j Then
                        Dim tri As New ManifoldFace(
                            structure.Points(startIdx1 + i),
                            structure.Points(startIdx2 + j),
                            structure.Points(startIdx1 + (i + 1) Mod 8))
                        
                        structure.Faces.Add(tri)
                    End If
                Next
            Next
        End Sub
    End Class
#End Region

#Region "流形操作和变换"
    Public Class ManifoldOperations
        ' 流形变换
        Public Shared Function TransformManifold(structure As ManifoldStructure,
                                                transform As Matrix4x4) As ManifoldStructure
            Dim transformed As New ManifoldStructure()
            
            ' 变换所有点
            For Each point In structure.Points
                Dim newPos = Vector3.Transform(point.Coordinates, transform)
                Dim newPoint = New ManifoldPoint(newPos)
                newPoint.Normal = Vector3.TransformNormal(point.Normal, transform)
                newPoint.TangentSpace = point.TangentSpace * transform
                transformed.Points.Add(newPoint)
            Next
            
            ' 复制面结构（使用变换后的点）
            For Each face In structure.Faces
                Dim v1Idx = structure.Points.IndexOf(face.Vertices(0))
                Dim v2Idx = structure.Points.IndexOf(face.Vertices(1))
                Dim v3Idx = structure.Points.IndexOf(face.Vertices(2))
                
                Dim newFace As New ManifoldFace(
                    transformed.Points(v1Idx),
                    transformed.Points(v2Idx),
                    transformed.Points(v3Idx))
                
                transformed.Faces.Add(newFace)
            Next
            
            ' 复制边
            transformed.Edges.AddRange(structure.Edges)
            
            Return transformed
        End Function
        
        ' 计算高斯曲率（近似）
        Public Shared Function CalculateGaussianCurvature(structure As ManifoldStructure) As Dictionary(Of Integer, Single)
            Dim curvature As New Dictionary(Of Integer, Single)()
            
            ' 初始化曲率为0
            For i As Integer = 0 To structure.Points.Count - 1
                curvature(i) = 0
            Next
            
            ' 计算每个面的角度缺陷
            For Each face In structure.Faces
                Dim v0 = structure.Points.IndexOf(face.Vertices(0))
                Dim v1 = structure.Points.IndexOf(face.Vertices(1))
                Dim v2 = structure.Points.IndexOf(face.Vertices(2))
                
                ' 计算角度
                Dim a = face.Vertices(0).Coordinates - face.Vertices(1).Coordinates
                Dim b = face.Vertices(2).Coordinates - face.Vertices(1).Coordinates
                Dim angle = Math.Acos(Vector3.Dot(Vector3.Normalize(a), Vector3.Normalize(b)))
                
                ' 将角度加到顶点
                curvature(v1) += angle
            Next
            
            ' 归一化曲率
            Dim maxCurv As Single = 0
            For Each kvp In curvature
                maxCurv = Math.Max(maxCurv, Math.Abs(kvp.Value))
            Next
            
            If maxCurv > 0 Then
                For i As Integer = 0 To curvature.Count - 1
                    curvature(i) /= maxCurv
                Next
            End If
            
            Return curvature
        End Function
        
        ' 流形细分
        Public Shared Function SubdivideManifold(structure As ManifoldStructure,
                                                levels As Integer) As ManifoldStructure
            Dim subdivided = structure
            
            For level As Integer = 1 To levels
                subdivided = SubdivideOnce(subdivided)
            Next
            
            Return subdivided
        End Function
        
        Private Shared Function SubdivideOnce(structure As ManifoldStructure) As ManifoldStructure
            Dim newStructure As New ManifoldStructure()
            
            ' 复制原始点
            For Each point In structure.Points
                newStructure.Points.Add(New ManifoldPoint(point.Coordinates))
            Next
            
            ' 细分每个面
            For Each face In structure.Faces
                Dim v0Idx = structure.Points.IndexOf(face.Vertices(0))
                Dim v1Idx = structure.Points.IndexOf(face.Vertices(1))
                Dim v2Idx = structure.Points.IndexOf(face.Vertices(2))
                
                ' 计算边的中点
                Dim m01 = (structure.Points(v0Idx).Coordinates + 
                          structure.Points(v1Idx).Coordinates) / 2
                Dim m12 = (structure.Points(v1Idx).Coordinates + 
                          structure.Points(v2Idx).Coordinates) / 2
                Dim m20 = (structure.Points(v2Idx).Coordinates + 
                          structure.Points(v0Idx).Coordinates) / 2
                
                ' 添加新点
                Dim m01Idx = newStructure.Points.Count
                newStructure.Points.Add(New ManifoldPoint(m01))
                
                Dim m12Idx = newStructure.Points.Count
                newStructure.Points.Add(New ManifoldPoint(m12))
                
                Dim m20Idx = newStructure.Points.Count
                newStructure.Points.Add(New ManifoldPoint(m20))
                
                ' 创建4个新三角形
                Dim tri1 As New ManifoldFace(
                    newStructure.Points(v0Idx),
                    newStructure.Points(m01Idx),
                    newStructure.Points(m20Idx))
                
                Dim tri2 As New ManifoldFace(
                    newStructure.Points(m01Idx),
                    newStructure.Points(v1Idx),
                    newStructure.Points(m12Idx))
                
                Dim tri3 As New ManifoldFace(
                    newStructure.Points(m20Idx),
                    newStructure.Points(m12Idx),
                    newStructure.Points(v2Idx))
                
                Dim tri4 As New ManifoldFace(
                    newStructure.Points(m01Idx),
                    newStructure.Points(m12Idx),
                    newStructure.Points(m20Idx))
                
                newStructure.Faces.AddRange({tri1, tri2, tri3, tri4})
            Next
            
            Return newStructure
        End Function
    End Class
#End Region

#Region "流形可视化器"
    Public Class ManifoldVisualizer
        Inherits Form
        
        Private WithEvents btnCreateManifold As New Button()
        Private WithEvents btnTransform As New Button()
        Private WithEvents cmbManifoldType As New ComboBox()
        Private WithEvents txtInfo As New TextBox()
        Private WithEvents pnlCanvas As New Panel()
        Private WithEvents trkRotation As New TrackBar()
        
        Private currentManifold As ManifoldStructure
        Private rotationAngle As Single = 0
        
        Public Sub New()
            InitializeComponent()
        End Sub
        
        Private Sub InitializeComponent()
            Me.Text = "三维流形可视化器"
            Me.Size = New Size(1200, 800)
            Me.BackColor = Color.FromArgb(20, 20, 30)
            
            ' 控制面板
            Dim controlPanel As New Panel()
            controlPanel.Location = New Point(20, 20)
            controlPanel.Size = New Size(1160, 120)
            controlPanel.BackColor = Color.FromArgb(40, 40, 60)
            
            ' 流形类型选择
            Dim lblType As New Label()
            lblType.Text = "流形类型："
            lblType.ForeColor = Color.White
            lblType.Location = New Point(20, 20)
            
            cmbManifoldType.Items.AddRange({"六方向流形", "球形流形", "环面流形", "双坐标系流形"})
            cmbManifoldType.SelectedIndex = 0
            cmbManifoldType.Location = New Point(100, 20)
            cmbManifoldType.Size = New Size(150, 25)
            
            ' 创建按钮
            btnCreateManifold.Text = "创建流形"
            btnCreateManifold.Location = New Point(270, 20)
            btnCreateManifold.Size = New Size(120, 30)
            btnCreateManifold.BackColor = Color.FromArgb(70, 130, 180)
            btnCreateManifold.ForeColor = Color.White
            
            ' 变换按钮
            btnTransform.Text = "应用变换"
            btnTransform.Location = New Point(400, 20)
            btnTransform.Size = New Size(120, 30)
            btnTransform.BackColor = Color.FromArgb(70, 180, 130)
            btnTransform.ForeColor = Color.White
            
            ' 旋转控制
            Dim lblRotation As New Label()
            lblRotation.Text = "旋转角度："
            lblRotation.ForeColor = Color.White
            lblRotation.Location = New Point(20, 60)
            
            trkRotation.Minimum = 0
            trkRotation.Maximum = 360
            trkRotation.Value = 0
            trkRotation.Location = New Point(100, 60)
            trkRotation.Size = New Size(200, 45)
            trkRotation.TickFrequency = 10
            AddHandler trkRotation.Scroll, AddressOf RotationChanged
            
            controlPanel.Controls.AddRange({lblType, cmbManifoldType, btnCreateManifold,
                                          btnTransform, lblRotation, trkRotation})
            
            ' 信息文本框
            txtInfo.Location = New Point(20, 160)
            txtInfo.Size = New Size(400, 600)
            txtInfo.Multiline = True
            txtInfo.ScrollBars = ScrollBars.Vertical
            txtInfo.BackColor = Color.FromArgb(15, 15, 25)
            txtInfo.ForeColor = Color.White
            txtInfo.Font = New Font("Consolas", 10)
            
            ' 画布面板
            pnlCanvas.Location = New Point(440, 160)
            pnlCanvas.Size = New Size(740, 600)
            pnlCanvas.BackColor = Color.Black
            pnlCanvas.BorderStyle = BorderStyle.FixedSingle
            
            Me.Controls.AddRange({controlPanel, txtInfo, pnlCanvas})
            
            ' 初始创建流形
            CreateManifold()
        End Sub
        
        Private Sub CreateManifold()
            Dim builder As New ManifoldBuilder()
            
            Select Case cmbManifoldType.SelectedItem.ToString()
                Case "球形流形"
                    currentManifold = builder.BuildSphericalManifold(3, 6, 20, 10)
                Case "环面流形"
                    currentManifold = builder.BuildTorusManifold(5, 2, 30, 15)
                Case "双坐标系流形"
                    currentManifold = builder.BuildDualCoordinateManifold()
                Case Else ' 六方向流形
                    currentManifold = builder.BuildSixDirectionManifold(3)
            End Select
            
            UpdateInfo()
            DrawManifold()
        End Sub
        
        Private Sub UpdateInfo()
            txtInfo.Clear()
            
            If currentManifold Is Nothing Then Exit Sub
            
            txtInfo.AppendText($"=== 流形信息 ===" & vbCrLf & vbCrLf)
            txtInfo.AppendText($"顶点数: {currentManifold.Points.Count}" & vbCrLf)
            txtInfo.AppendText($"面数: {currentManifold.Faces.Count}" & vbCrLf)
            txtInfo.AppendText($"边数: {currentManifold.Edges.Count}" & vbCrLf & vbCrLf)
            
            ' 边界框
            Dim bbox = currentManifold.GetBoundingBox()
            txtInfo.AppendText($"边界框：" & vbCrLf)
            txtInfo.AppendText($"  最小: {bbox.Min.X:F2}, {bbox.Min.Y:F2}, {bbox.Min.Z:F2}" & vbCrLf)
            txtInfo.AppendText($"  最大: {bbox.Max.X:F2}, {bbox.Max.Y:F2}, {bbox.Max.Z:F2}" & vbCrLf & vbCrLf)
            
            ' 体积
            Dim volume = currentManifold.CalculateVolume()
            txtInfo.AppendText($"近似体积: {volume:F4}" & vbCrLf & vbCrLf)
            
            ' 显示前几个点的坐标
            txtInfo.AppendText("前5个顶点坐标：" & vbCrLf)
            For i As Integer = 0 To Math.Min(4, currentManifold.Points.Count - 1)
                txtInfo.AppendText($"  {i}: {currentManifold.Points(i)}" & vbCrLf)
            Next
        End Sub
        
        Private Sub DrawManifold()
            If currentManifold Is Nothing Then Exit Sub
            
            Dim g As Graphics = pnlCanvas.CreateGraphics()
            g.Clear(Color.Black)
            
            ' 设置3D投影参数
            Dim width As Integer = pnlCanvas.Width
            Dim height As Integer = pnlCanvas.Height
            Dim centerX As Integer = width \ 2
            Dim centerY As Integer = height \ 2
            Dim scale As Single = 20.0F
            
            ' 应用旋转
            Dim rotation = Matrix4x4.CreateRotationY(rotationAngle * Math.PI / 180.0F)
            
            ' 绘制所有边
            Dim penEdge As New Pen(Color.FromArgb(150, 100, 200, 255), 2)
            
            For Each edge In currentManifold.Edges
                Dim p1 = currentManifold.Points(edge.Item1).Coordinates
                Dim p2 = currentManifold.Points(edge.Item2).Coordinates
                
                ' 应用旋转
                p1 = Vector3.Transform(p1, rotation)
                p2 = Vector3.Transform(p2, rotation)
                
                ' 3D到2D投影（简单透视）
                Dim x1 = centerX + p1.X * scale
                Dim y1 = centerY + p1.Z * scale ' 使用Z作为垂直方向
                Dim x2 = centerX + p2.X * scale
                Dim y2 = centerY + p2.Z * scale
                
                g.DrawLine(penEdge, x1, y1, x2, y2)
            Next
            
            ' 绘制顶点
            Dim brushVertex As New SolidBrush(Color.Yellow)
            For Each point In currentManifold.Points
                Dim pos = Vector3.Transform(point.Coordinates, rotation)
                
                Dim x = centerX + pos.X * scale
                Dim y = centerY + pos.Z * scale
                
                g.FillEllipse(brushVertex, x - 3, y - 3, 6, 6)
            Next
            
            ' 绘制面（带透明填充）
            Dim colors() As Color = {
                Color.FromArgb(50, 255, 0, 0),
                Color.FromArgb(50, 0, 255, 0),
                Color.FromArgb(50, 0, 0, 255),
                Color.FromArgb(50, 255, 255, 0),
                Color.FromArgb(50, 255, 0, 255),
                Color.FromArgb(50, 0, 255, 255)
            }
            
            For faceIndex As Integer = 0 To Math.Min(19, currentManifold.Faces.Count - 1)
                Dim face = currentManifold.Faces(faceIndex)
                Dim colorIndex = faceIndex Mod colors.Length
                
                Dim brushFace As New SolidBrush(colors(colorIndex))
                
                Dim points(2) As PointF
                For i As Integer = 0 To 2
                    Dim pos = Vector3.Transform(face.Vertices(i).Coordinates, rotation)
                    points(i) = New PointF(
                        centerX + pos.X * scale,
                        centerY + pos.Z * scale)
                Next
                
                g.FillPolygon(brushFace, points)
                brushFace.Dispose()
            Next
            
            ' 绘制坐标轴
            DrawAxes(g, centerX, centerY, scale, rotation)
            
            ' 绘制标题
            Dim brushText As New SolidBrush(Color.White)
            g.DrawString($"流形类型: {cmbManifoldType.SelectedItem}", 
                        New Font("Arial", 14, FontStyle.Bold), 
                        brushText, 10, 10)
            
            g.DrawString($"旋转角度: {rotationAngle}°", 
                        New Font("Arial", 12), 
                        brushText, 10, 40)
            
            ' 清理资源
            penEdge.Dispose()
            brushVertex.Dispose()
            brushText.Dispose()
        End Sub
        
        Private Sub DrawAxes(g As Graphics, centerX As Integer, centerY As Integer,
                            scale As Single, rotation As Matrix4x4)
            Dim axisLength As Single = 50
            
            ' X轴 (红色)
            Dim xEnd = Vector3.Transform(New Vector3(axisLength, 0, 0), rotation)
            g.DrawLine(New Pen(Color.Red, 3),
                      centerX, centerY,
                      centerX + xEnd.X * scale, centerY + xEnd.Z * scale)
            g.DrawString("X", New Font("Arial", 12), Brushes.Red,
                        centerX + xEnd.X * scale + 5, centerY + xEnd.Z * scale)
            
            ' Y轴 (绿色)
            Dim yEnd = Vector3.Transform(New Vector3(0, axisLength, 0), rotation)
            g.DrawLine(New Pen(Color.Green, 3),
                      centerX, centerY,
                      centerX + yEnd.X * scale, centerY + yEnd.Z * scale)
            g.DrawString("Y", New Font("Arial", 12), Brushes.Green,
                        centerX + yEnd.X * scale + 5, centerY + yEnd.Z * scale)
            
            ' Z轴 (蓝色)
            Dim zEnd = Vector3.Transform(New Vector3(0, 0, axisLength), rotation)
            g.DrawLine(New Pen(Color.Blue, 3),
                      centerX, centerY,
                      centerX + zEnd.X * scale, centerY + zEnd.Z * scale)
            g.DrawString("Z", New Font("Arial", 12), Brushes.Blue,
                        centerX + zEnd.X * scale + 5, centerY + zEnd.Z * scale)
            
            ' 原点
            g.FillEllipse(Brushes.White, centerX - 4, centerY - 4, 8, 8)
            g.DrawString("O", New Font("Arial", 12), Brushes.White,
                        centerX + 5, centerY + 5)
        End Sub
        
        Private Sub RotationChanged(sender As Object, e As EventArgs)
            rotationAngle = trkRotation.Value
            DrawManifold()
        End Sub
        
        Protected Overrides Sub OnPaint(e As PaintEventArgs)
            MyBase.OnPaint(e)
            DrawManifold()
        End Sub
        
        ' 事件处理
        Private Sub btnCreateManifold_Click(sender As Object, e As EventArgs) Handles btnCreateManifold.Click
            CreateManifold()
        End Sub
        
        Private Sub btnTransform_Click(sender As Object, e As EventArgs) Handles btnTransform.Click
            If currentManifold Is Nothing Then Exit Sub
            
            ' 应用一些变换
            Dim transform = Matrix4x4.CreateRotationX(Math.PI / 4) *
                           Matrix4x4.CreateRotationY(Math.PI / 3) *
                           Matrix4x4.CreateScale(1.2F, 1.2F, 1.2F)
            
            currentManifold = ManifoldOperations.TransformManifold(currentManifold, transform)
            
            UpdateInfo()
            DrawManifold()
        End Sub
    End Class
#End Region

#Region "主程序入口"
    Module MainModule
        <STAThread>
        Sub Main()
            Application.EnableVisualStyles()
            Application.SetCompatibleTextRenderingDefault(False)
            Application.Run(New ManifoldVisualizer())
        End Sub
    End Module
#End Region
End Class