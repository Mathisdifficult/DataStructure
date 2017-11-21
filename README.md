# DataStructure
数据结构课程设计实验报告
姓名：_____________  学号：_____________  专业：_______________


题目说明：
	利用带头结点的链表结构，开发一个一元多项式的运算程序，要求程序具备以下操作接口：
1.CreatPolyn（多项式创建函数）；
2.PrintPolyn（多项式打印函数）；
3.AddPolyn（多项式相加函数）；
4.Opposite（多项式取反函数）；
5.SubtractPolyn（多项式相减函数）；
6.MultiplyPolyn（多项式相乘函数）；

要求程序具备用户选择操作菜单，支持以下菜单项：
1.显示多项式；
2.多项式相加；
3.多项式相减；
4.多项式相乘；
5.多项式取反；
6.退出程序。

注意：
1.程序应支持次数乱序输入
2.程序应支持相同次数同类项合并
3.输入多项式时，按照“系数 次数”的形式依次输入







功能模块结构说明表（以顺序表结构为例）
函数名	参数说明	返回值说明	操作行为说明
Create		返回多项式的链表	创建多项式
Sort	
参数1：传入多项式的链表	
无返回值	   按幂次降排序
Show	参数1：传入多项式的链表	
无返回值	显示多项式
Power	参数1：x的值
参数2：对应项的次数	部分多项式的值	计算部分多项式的值
Value	参数1：多项式
参数2：对应x的值	多项式的值	计算多项式的值
Copy	参数1：多项式	多项式	复制多项式
Additive	参数1：多项式A
参数2：多项式B	多项式	多项式A+B
Subtract	参数1：多项式A
参数2：多项式B	多项式	多项式A-B
Multiplication	参数1：多项式A
参数2：多项式B	多项式	多项式A*B
GetOpposite	参数1：多项式	多项式	多项式取反
ListLength	参数1：多项式	多项式的长度	多项式的长度
Merge	参数1：多项式	合并后的多项式	多项式合并
FreeMemory	参数1：多项式	无返回值	释放内存
main			主函数





缺陷记录表（以顺序表结构为例）
序号	程序缺陷说明	修复情况	备注
1	取反后，多项式也会取反	已修复	严重bug必须修复，不直接在原多项式进行操作。

2	项数小于等于2程序会崩溃	已修复	加入判断条件。
3	多项式有时候不会合并	已修复	先冒泡排序，再对前后次数进行判断
4	程序结束后释放内存	已修复	强迫症
	