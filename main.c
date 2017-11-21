#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#define EPS 1E-8

typedef struct item {
    double coefficient;
    int power;
    struct item *next;
} *POLYNOMIAL,*pItem;

POLYNOMIAL Create() { // ��������ʽ
    pItem head,p;
    double coe;
    int pwr,iterms,i;
    head = p = (pItem)malloc(sizeof(struct item));
    scanf("%d",&iterms); // �������ʽ������
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

void Sort(POLYNOMIAL head) { // ���ݴν�����
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

void Show(POLYNOMIAL head) { // ��ʾ
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

double Value(POLYNOMIAL head,double x) { // ����ʽ��ֵ
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

POLYNOMIAL Additive(POLYNOMIAL A, POLYNOMIAL B) { // ����ʽ��
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

POLYNOMIAL Subtract(POLYNOMIAL A, POLYNOMIAL B) { // ����ʽ��
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

POLYNOMIAL Multiplication(POLYNOMIAL A, POLYNOMIAL B) { // ����ʽ��
    POLYNOMIAL head,t,p,q;
    head = t = (pItem)malloc(sizeof(struct item));
    for(p = A->next; p; p = p->next) { // �����˹���
        for(q = B->next; q; q = q->next) {
            t->next = (pItem)malloc(sizeof(struct item));
            t->next->coefficient = p->coefficient * q->coefficient;
            t->next->power = p->power + q->power;
            t = t->next;
        }
    }
    t->next = NULL;
    Sort(head); // ����
    p = head;
    while(p->next) { // �ϲ�ͬ����
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
    POLYNOMIAL p=L->next; /* pָ���һ����� */
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
    printf("��������ʽA:\n");
    printf("����ʽA������:");
    A = Create();
    Merge(A);
    Sort(A);
    printf("A(x) = ");Show(A);
    printf("��������ʽB:\n");
    printf("����ʽB������:");
    B = Create();
    Merge(B);
    Sort(B);
    printf("B(x) = ");Show(B);
    next:printf(">>>>>1. ��ʾ����ʽ<<<<<<<<<\n>>>>>2. ����ʽ���<<<<<<<<<\n>>>>>3. ����ʽ���<<<<<<<<<\n>>>>>4. ����ʽ���<<<<<<<<<\n>>>>>5. ����ʽA(x)ȡ��<<<<<\n>>>>>6. ����ʽB(x)ȡ��<<<<<\n>>>>>7. ��x��ֵ������<<<<<<\n>>>>>8. �˳�����<<<<<<<<<<<\n����������Ϊ");
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
        printf("������x����ֵ\n");
        int x;
        scanf("%d",&x);
        printf("A(%d)=%lf\n",x,Value(A,x));
        printf("B(%d)=%lf",x,Value(B,x));
        break;
        case 8 :
        printf("�ݰ���\n");
        break;
        default  :
        printf("����ʶ������ : %d\n",i);
        goto next;
    }
   // }

    FreeMemory(A);
    FreeMemory(B);
    return 0;
}
