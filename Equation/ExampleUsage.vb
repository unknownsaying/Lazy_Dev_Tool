Imports System
Imports System.Windows.Forms
Imports DeploymentAutomationGUI.Converters
Imports DeploymentAutomationGUI.Commands
Imports DeploymentAutomationGUI.Extensions
Imports DeploymentAutomationGUI.Core

Public Class ExampleUsage
    Private Sub Example1()
        ' 1. 使用转换器
        Dim converter As New BooleanToVisibilityConverter()
        Dim visible = converter.Convert(True, GetType(Visibility), Nothing, Nothing)
        ' visible = Visibility.Visible
        
        ' 2. 使用命令
        Dim command = New RelayCommand(
            Sub(param) MessageBox.Show("Command executed!"),
            Function(param) True)
        
        If command.CanExecute(Nothing) Then
            command.Execute(Nothing)
        End If
        
        ' 3. 使用扩展方法
        Dim text = "Hello World"
        Dim camelCase = text.ToCamelCase()           ' "helloWorld"
        Dim pascalCase = text.ToPascalCase()         ' "HelloWorld"
        Dim snakeCase = text.ToSnakeCase()           ' "hello_world"
        Dim kebabCase = text.ToKebabCase()           ' "hello-world"
        Dim isEmail = "test@example.com".IsValidEmail() ' True
        
        ' 4. 使用子程序
        Utility.CleanupTempDirectory("C:\Temp", 7)
        
        ' 5. 使用函数
        Dim uniqueName = Utility.GenerateUniqueFileName("test.txt", "C:\Temp")
        Dim isValid = Utility.IsValidEmail("test@example.com")
        
        ' 6. 使用配置管理器
        ConfigurationManager.Instance.SetValue("Username", "JohnDoe")
        Dim username = ConfigurationManager.Instance.GetValue(Of String)("Username", "Guest")
        
        ' 7. 使用验证助手
        Dim result = ValidationHelper.ValidateEmail("test@example.com", "Email")
        If result.IsValid Then
            Console.WriteLine("Email is valid")
        Else
            Console.WriteLine($"Error: {result.ErrorMessage}")
        End If
    End Sub
    
    Private Async Sub ExampleAsync()
        ' 异步命令示例
        Dim asyncCommand = New AsyncRelayCommand(
            Async Function(param)
                Await Task.Delay(1000)
                MessageBox.Show("Async command completed!")
                Return True
            End Function)
        
        ' 执行异步命令
        asyncCommand.Execute(Nothing)
        
        ' 异步配置保存
        Await ConfigurationManager.Instance.SaveAsync()
        
        ' 使用异步实用函数
        Await Utility.DelayAsync(2000)
    End Sub
    
    Private Sub ExampleCollections()
        ' 使用集合扩展方法
        Dim numbers = New List(Of Integer) From {1, 2, 3, 4, 5}
        
        ' 检查集合是否为空
        If Not numbers.IsNullOrEmpty() Then
            Console.WriteLine($"Count: {numbers.Count}")
        End If
        
        ' 随机选择一个元素
        Dim randomNumber = numbers.RandomElement()
        
        ' 安全遍历
        numbers.ForEachSafe(Sub(n) Console.WriteLine(n))
        
        ' 转换为只读集合
        Dim readOnlyNumbers = numbers.AsReadOnly()
        
        ' 分块处理
        For Each chunk In numbers.Chunk(2)
            Console.WriteLine($"Chunk: {String.Join(", ", chunk)}")
        Next
    End Sub
    
    Private Sub ExampleIO()
        ' 使用IO扩展方法
        Dim filePath = "C:\test.txt"
        
        ' 安全读取文件
        Dim content = filePath.ReadAllTextSafe()
        
        ' 安全写入文件
        filePath.WriteAllTextSafe("Hello World")
        
        ' 获取文件信息
        Dim size = filePath.GetHumanReadableSize()
        Dim mimeType = filePath.GetMimeType()
        Dim isText = filePath.IsTextFile()
        
        Console.WriteLine($"File: {filePath}")
        Console.WriteLine($"Size: {size}")
        Console.WriteLine($"MIME Type: {mimeType}")
        Console.WriteLine($"Is Text: {isText}")
    End Sub
End Class