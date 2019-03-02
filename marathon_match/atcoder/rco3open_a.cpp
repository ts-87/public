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
    //cout<<0<<endl;
    int mv=1;
    while(1){
        if(flag){
            cout<<cur<<endl;
            cin>>cn;
        }
        
        int mn=INF,id=-1;//,nid=-1;
        /*
        for(int i=cur+mv;i>=0&&i<N;i+=mv){
            if(num[i]==-1){
                nid
            }
        }
        */
        rep(i,N){
            if(cur!=i&&num[i]==cn&&abs(cur-i)<mn&&abs(cur-i)<=dist){
                mn=abs(cur-i);
                id=i;
            }
        }
        if(id!=-1){
            /*
            int mv2=1;
            if(cur>id) mv2=-1;
            else mv2=1;
            */
            score+=cn;
            num[id]=-1;
            num[cur]=-1;
            cur=id;
            int tot=0,cnt=0;
            FOR(i,cur,N){
                if(num[i]==-1) ++tot;
                if(num[i]==-1&&cur>=i) ++cnt;
            }
            if(tot-cnt<=cnt) mv=1;
            else mv=-1;
            flag=true;
        }
        else{
            num[cur]=cn;
            int nid=-1;
            mn=INF;
            rep(i,N){
                if(num[i]==-1&&abs(cur-i)<mn&&abs(cur-i)<=dist){
                    mn=abs(cur-i);
                    nid=i;
                }
            }
            if(nid!=-1){
                while(1){
                    if(cur==N-1)mv=-1;
                    else if(cur==0) mv=1;
                    cur+=mv;
                    if(num[cur]==-1) break;
                }
                cur=nid;
            }
            else{
                if(cur==N-1)mv=-1;
                else if(cur==0) mv=1;
                cur+=mv;
            }
            flag=false;
        }
        if(dist<=0){
            cout<<-1<<endl;
            break;
        }
        else{
            if(mn==INF)dist-=1;
            else dist-=mn;
            cout<<cur<<endl;
            cin>>cn;
        }
    }
    cerr<<score<<endl;
    return 0;
}
