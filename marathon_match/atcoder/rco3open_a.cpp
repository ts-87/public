#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

int N,T;
int INF=1000100010;
int num[51];
int cn=0,cur,score=0,dist=0,pre=-1;
int main(){
    cin>>N>>T;
    dist=T;
    cur=0;pre=0;
    memset(num,-1,sizeof(num));
    bool flag=true;
    int fac=6;
    while(1){
        if(flag){
            cout<<cur<<endl;
            cin>>cn;
            num[cur]=cn;
        }
        if(dist<50) fac=30;
        int mn=INF,nm=INF,id=-1,di=-1;
        //int cnt=0;
        rep(i,N){
            if(cur!=i&&num[i]==-1&&abs(cur-i)<nm&&abs(cur-i)<=dist){
                nm=abs(cur-i);
                di=i;
            }
            //if(num[i]==-1) ++cnt;
        }
        rep(i,N){
            if(cur!=i&&num[i]==cn&&abs(cur-i)<mn&&abs(cur-i)<=dist){
                mn=abs(cur-i);
                id=i;
            }
        }
        if((di==-1&&id!=-1)||mn<10||(di!=-1&&(mn<nm*fac))){
            score+=cn;
            num[id]=-1;
            num[cur]=-1;
            cur=id;
            cout<<cur<<endl;
            cin>>cn;
            dist-=mn;
            flag=true;
        }
        else if(di!=-1){
            int pre=cn;
            cout<<di<<endl;
            cin>>cn;
            if(pre==cn){
                score+=cn;
                num[di]=-1;
                num[cur]=-1;
                flag=true;
            }
            else{
                num[di]=cn;
                flag=false;
            }
            dist-=nm;
            cur=di;
        }
        else{
            cout<<-1<<endl;
            break;
        }
        if(dist<=0){
            cout<<-1<<endl;
            break;
        }
    }
    cerr<<score<<endl;
    return 0;
}
