#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#define EPS 1E-8

typedef struct item {
    double coefficient;
    int power;
    struct item *next;
} *POLYNOMIAL,*pItem;

POLYNOMIAL Create() { // 创建多项式
    pItem head,p;
    double coe;
    int pwr,iterms,i;
    head = p = (pItem)malloc(sizeof(struct item));
    scanf("%d",&iterms); // 读入多项式的项数
    for(i = 0; i < iterms; ++i) {
        scanf("%lf%d",&coe,&pwr);
        p->next = (pItem)malloc(sizeof(struct item));
        p->next->coefficient = coe;
        p->next->power = pwr;
        p = p->next;
    }
    p->next = NULL;
    return head;
}

void Sort(POLYNOMIAL head) { // 按幂次降排序
    pItem pt,q,p = head;
    while(p->next) {
        q = p->next;
        while(q->next) {
            if(p->next->power < q->next->power) {
                pt = p->next;
                p->next = q->next;
                q->next = p->next->next;
                p->next->next = pt;
            }
            else q = q->next;
        }
        p = p->next;
    }
}

void Show(POLYNOMIAL head) { // 显示
    POLYNOMIAL p = head->next;
    int flag = 1;
    if(p == NULL) return;
    while(p) {
        if(flag) {
            if(fabs(p->coefficient) >= EPS) {
                if(p->power == 0) printf("%.2lf",p->coefficient);
                else if(p->power == 1) {
                    if(p->coefficient == 1.0) printf("x ");
                    else if(p->coefficient == -1.0) printf("-x ");
                    else printf("%.2lfx ",p->coefficient);
                }
                else if(p->coefficient == 1.0) printf("x^%d ",p->power);
                else if(p->coefficient == -1.0) printf("-x^%d ",p->power);
                else printf("%.2lfx^%d ",p->coefficient,p->power);
                flag = 0;
            }
        }
        else if(p->coefficient > 0.0 && fabs(p->coefficient) >= EPS) {
            if(p->power == 0) printf("+ %.2lf ",p->coefficient);
            else if(p->power == 1) {
                if(p->coefficient == 1.0) printf("+ x ");
                else printf("+ %.2lfx ",p->coefficient);
            }
            else if(p->coefficient == 1.0) printf("+ x^%d ",p->power);
            else printf("+ %.2lfx^%d ",p->coefficient,p->power);
        }
        else if(p->coefficient < 0.0 && fabs(p->coefficient) >= EPS) {
            if(p->power == 0) printf("- %.2lf ",-p->coefficient);
            else if(p->power == 1) {
                if(p->coefficient == -1.0) printf("- x ");
                else printf("- %.2lfx ",-p->coefficient);
            }
            else if(p->coefficient == -1.0) printf("- x^%d ",-p->power);
            else printf("- %.2lfx^%d ",-p->coefficient,p->power);
        }
        p = p->next;
    }
    printf("\n");
}

double Power(double x,int n) {
    double value = 1.0;
    int i;
    for(i = 0; i < n; ++i) value *= x;
        return value;
}

double Value(POLYNOMIAL head,double x) { // 多项式求值
    POLYNOMIAL p;
    double value = 0.0;
    for(p = head->next; p; p = p->next)
        value += p->coefficient * Power(x,p->power);
    return value;
}

POLYNOMIAL Copy(POLYNOMIAL A) {
    POLYNOMIAL head,t,p;
    head = t = (pItem)malloc(sizeof(struct item));
    for(p = A->next; p; p = p->next) {
        t->next = (pItem)malloc(sizeof(struct item));
        t->next->coefficient = p->coefficient;
        t->next->power = p->power;
        t = t->next;
    }
    t->next = NULL;
    return head;
}

POLYNOMIAL Additive(POLYNOMIAL A, POLYNOMIAL B) { // 多项式加
    POLYNOMIAL head,p,q,t;
    head = Copy(A);
    for(p = B; p->next; p = p->next) {
        q = head;
        while(q->next) {
            if(p->next->power == q->next->power) {
                q->next->coefficient += p->next->coefficient;
                if(fabs(q->next->coefficient) <= EPS) {
                    t = q->next;
                    q->next = t->next;
                    free(t);
                }
                break;
            }
            q = q->next;
        }
        if(q->next == NULL) {
            q->next = (pItem)malloc(sizeof(struct item));
            q->next->coefficient = p->next->coefficient;
            q->next->power = p->next->power;
            q->next->next = NULL;
        }
    }
    Sort(head);
    return head;
}

POLYNOMIAL Subtract(POLYNOMIAL A, POLYNOMIAL B) { // 多项式减
    POLYNOMIAL head,p,q,t;
    head = Copy(A);
    for(p = B; p->next; p = p->next) {
        q = head;
        while(q->next) {
            if(p->next->power == q->next->power) {
                q->next->coefficient -= p->next->coefficient;
                if(fabs(q->next->coefficient) <= EPS) {
                    t = q->next;
                    q->next = t->next;
                    free(t);
                }
                break;
            }
            q = q->next;
        }
        if(q->next == NULL) {
            q->next = (pItem)malloc(sizeof(struct item));
            q->next->coefficient = -p->next->coefficient;
            q->next->power = p->next->power;
            q->next->next = NULL;
        }
    }
    Sort(head);
    return head;
}

POLYNOMIAL Multiplication(POLYNOMIAL A, POLYNOMIAL B) { // 多项式乘
    POLYNOMIAL head,t,p,q;
    head = t = (pItem)malloc(sizeof(struct item));
    for(p = A->next; p; p = p->next) { // 完成相乘过程
        for(q = B->next; q; q = q->next) {
            t->next = (pItem)malloc(sizeof(struct item));
            t->next->coefficient = p->coefficient * q->coefficient;
            t->next->power = p->power + q->power;
            t = t->next;
        }
    }
    t->next = NULL;
    Sort(head); // 排序
    p = head;
    while(p->next) { // 合并同类项
        q = p->next;
        while(q->next) {
            if(p->next->power == q->next->power) {
                p->next->coefficient += q->next->coefficient;
                t = q->next;
                q->next = t->next;
                free(t);
            }
            else q = q->next;
        }
        p = p->next;
    }
    return head;
}

POLYNOMIAL GetOpposite(POLYNOMIAL A)   {

    POLYNOMIAL p,q;
    p= Copy(A);
    q=p;
    while(q->next){
        q->next->coefficient=q->next->coefficient*(-1);
        q=q->next;
    }

   // return p;
    Sort(p);
    return p;
   // Show(p);
}
int ListLength(POLYNOMIAL L)
{
    int i=0;
    POLYNOMIAL p=L->next; /* p指向第一个结点 */
    while(p)
    {
        i++;
        p=p->next;
    }
    return i;
}
POLYNOMIAL Merge(POLYNOMIAL A){
     struct item*pm;
     pm=A->next;
     int size=ListLength(A);
     int i;
     for ( i = 0; i < size-1; i++){
        while(pm->next!=NULL){
                   if (pm->power > pm->next->power) {

                    int temp,temp1;
                    temp = pm->power;
                    pm->power = pm->next->power;
                    pm->next->power = temp;
                    temp1 = pm->coefficient;
                    pm->coefficient = pm->next->coefficient;
                    pm->next->coefficient = temp1;
                }
                 else if(pm->power== pm->next->power){
                      pm->coefficient+=pm->next->coefficient;
                        POLYNOMIAL li;
                        if(pm->next->next){
                        li=pm->next->next;
                        free(pm->next);
                        pm->next=li;
                        }
                        else{
                           free(pm->next);
                           pm->next=NULL;
                           break;
                        }

                }

                pm = pm->next;
            }
            pm = A->next;
        }

  return A;
}
void FreeMemory(POLYNOMIAL head) {
    POLYNOMIAL q,p = head;
    while(p) {
        q = p;
        p = q->next;
        free(q);
    }
}

int main() {
   // char ops[5];
    system("color 2f");
    int i;
    POLYNOMIAL A,B,C = NULL,D = NULL,E = NULL,F = NULL,G = NULL;
    printf("创建多项式A:\n");
    printf("多项式A的项数:");
    A = Create();
    Merge(A);
    Sort(A);
    printf("A(x) = ");Show(A);
    printf("创建多项式B:\n");
    printf("多项式B的项数:");
    B = Create();
    Merge(B);
    Sort(B);
    printf("B(x) = ");Show(B);
    next:printf(">>>>>1. 显示多项式<<<<<<<<<\n>>>>>2. 多项式相加<<<<<<<<<\n>>>>>3. 多项式相减<<<<<<<<<\n>>>>>4. 多项式相乘<<<<<<<<<\n>>>>>5. 多项式A(x)取反<<<<<\n>>>>>6. 多项式B(x)取反<<<<<\n>>>>>7. 给x赋值并计算<<<<<<\n>>>>>8. 退出程序<<<<<<<<<<<\n你输入的序号为");
    fflush(stdin);
   // gets(ops);
    scanf("%d",&i);

  //  for(int i = 0; ops[i]; ++i) {
    switch(i) {
        case 1 :
        printf("A(x) = ");Show(A);
        printf("B(x) = ");Show(B);
        goto next;
        case 2 : C = Additive(A,B);
        printf("A(x)+B(x)=");
        Show(C);
        printf("\n");
        goto next;
        case 3 : D = Subtract(A,B);
        printf("A(x)-B(x)=");
        Show(D);
        printf("\n");
        goto next;
        case 4 : E = Multiplication(A,B);
        printf("A(x)*B(x)=");
        Show(E);
        printf("\n");
        goto next;
        case 5 : F = GetOpposite(A);
        printf("Opposite A(x) = ");
        Show(F);
        printf("\n");
        goto next;
        case 6 : G = GetOpposite(B);
        printf("Opposite B(x) = ");
        Show(G);
        printf("\n");
        goto next;
        case 7 :
        printf("请输入x的数值\n");
        int x;
        scanf("%d",&x);
        printf("A(%d)=%lf\n",x,Value(A,x));
        printf("B(%d)=%lf",x,Value(B,x));
        break;
        case 8 :
        printf("拜拜了\n");
        break;
        default  :
        printf("不能识别该序号 : %d\n",i);
        goto next;
    }
   // }

    FreeMemory(A);
    FreeMemory(B);
    return 0;
}
