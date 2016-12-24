# DL Compile
## Draw Language Compile Tools
## 西安电子科技大学软件学院2014级编译原理上机作业

### 开发环境
> .NET Core 1.0.0-preview2-1-003177

> Visual Studio Code 1.8.0

### 文件清单
> Type.fs：项目中使用的类型定义，异常类型，全局变量和辅助函数

> FileIO.fs：从文件中读取源代码、去除注释，生成SVG文件内容，生成SVG文件

> Tokenizer.fs：词法解析器，从源代码生成 Token List

> Parser.fs：语法语义分析器，从 Token List 生成 AST

> Transformation.fs：解释器，处理 AST

> Draw.fs：调用解释器解释处理后的 AST 并生成 SVG 中的 Rect节点

> Program.fs：程序主入口

> test.dl：用于测试的绘图语言源代码

> test.svg：生成的测试 SVG 图片

#### 说明
> 使用 F# 语言开发

> 仅使用.NET Core 核心类库，未添加其他第三方引用

> 需要 .NET Core 运行环境

> Build 下提供可直接在 Windows 上运行的二进制文件

> 使用说明<br/>
Usage: **(dotnet run)/(compile.exe)** *[CodeFile]* *[Commands]* <br/>
&nbsp;&nbsp;Arguments:<br/>
&nbsp;&nbsp;&nbsp;&nbsp;*[CodeFile]*&nbsp;&nbsp;&nbsp;&nbsp;The Draw Language Source Code File <br/>
&nbsp;&nbsp;&nbsp;&nbsp;*[Commands]*&nbsp;&nbsp;&nbsp;&nbsp;The Run Options <br/>
&nbsp;&nbsp;Run Options: <br/>
&nbsp;&nbsp;&nbsp;&nbsp;-h &nbsp;&nbsp;&nbsp;&nbsp; Show Help <br/>
&nbsp;&nbsp;&nbsp;&nbsp;-c &nbsp;&nbsp;&nbsp;&nbsp; Show The Processed Code Source <br/>
&nbsp;&nbsp;&nbsp;&nbsp;-t &nbsp;&nbsp;&nbsp;&nbsp; Show The Token List <br/>
&nbsp;&nbsp;&nbsp;&nbsp;-a &nbsp;&nbsp;&nbsp;&nbsp; Show The Primitive AST <br/>
&nbsp;&nbsp;&nbsp;&nbsp;-n &nbsp;&nbsp;&nbsp;&nbsp; Show The Static Handle AST <br/>
&nbsp;&nbsp;&nbsp;&nbsp;-r &nbsp;&nbsp;&nbsp;&nbsp; Show The Rect List <br/>
&nbsp;&nbsp;&nbsp;&nbsp;-s &nbsp;&nbsp;&nbsp;&nbsp; Show The SVG File Content <br/>
&nbsp;&nbsp;&nbsp;&nbsp;-o &nbsp;&nbsp;&nbsp;&nbsp; Build The SVG File <br/>

> 初次使用 F# ， 如有错误或建议恳请发送邮件至[starsriver@outlook.com](starsriver@outlook.com)

>PS : GitHub 中插入SVG会显示不全，下图是生成的test.svg导出的png图片




![我跟你说我就这个表情](test.png)