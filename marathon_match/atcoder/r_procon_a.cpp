//https://atcoder.jp/contests/rcl-contest-2020-final/submissions/10649402
//を参考
#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

const int64_t CYCLES_PER_SEC=2800000000;
struct Timer {
	int64_t start;
	Timer() { reset(); }
	void reset() { start = getCycle(); }
	inline double get() { return (double)(getCycle() - start) / CYCLES_PER_SEC; }
	inline int64_t getCycle() {
		uint32_t low, high;
		__asm__ volatile ("rdtsc" : "=a" (low), "=d" (high));
		return ((int64_t)low) | ((int64_t)high << 32);
	}
};

int N,M;//500,5000
int V[501],pos=0,score=0,pre=0;
int cv[501],gcnt=0;
int d,v;
int nd[6]={1,2,3,4,5,6};
int nc[7]={0,1,1,1,1,1,1};
int ls=30;
int pena=200;//1000000/M
int cnd[]={0,5,4,3};
double dp[507][5][6][6];//num1,1-6(id0),1-6(id1)
int nex[507][5][6][6];
const int INF=500000;
int main(){
    Timer timer;
    cin>>N>>M;
    bool dec=false;
    memset(V,-1,sizeof(V));
    V[N-1]=0,V[N-2]=0,V[N-3]=0,V[N-4]=0,V[N-5]=0;
    int turn=0;
    for(;;++turn){
        dec=false;
        if(N-pos<6){
            ++nc[N-pos];--nc[nd[turn%2]];
            nd[turn%2]=N-pos;
            dec=true;
        }
        else if(N-pos<ls){
            if(nc[6]<=5){
                rep(j,6)if(nd[j]!=6){
                    ++nc[6];--nc[nd[j]];
                    nd[j]=6;
                    dec=true;
                    break;
                }
            }
            else if((N-pos)%6!=0){
                int nx=6-(N-pos)%6;
                rep(j,6)if(nc[6]==6||nd[j]!=6){
                    ++nc[nx];--nc[nd[j]];
                    nd[j]=nx;
                    dec=true;
                    break;
                }
            }
            else if((N-pos)%6==0){
                rep(j,6)if(nd[j]!=6){
                    ++nc[6];--nc[nd[j]];
                    nd[j]=6;
                    break;
                }
                dec=true;
            }
        }
        if(!dec){
        if(turn<6){
            ++nc[6];--nc[nd[turn%6]];
            nd[turn%6]=6;
        }
        else if(V[pre]>=125){
            --nc[nd[0]];++nc[1];
            nd[0]=1;
        }
        else{
            ++nc[6];--nc[nd[0]];
            nd[0]=6;
        }
        }
        
        rep(j,6){
            cout<<nd[j]<<" "; 
        }
        cout<<endl;
        pre=pos;
        cin>>d>>v>>pos;
        score+=v;
        if(V[pos]==-1) V[pos]=v;
        if(pos==N){
            pos=0,++gcnt;++turn;
            break;
        }
    }

    int I1=0,I2,I3;
    I2=nd[0]-1,I3=nd[1]-1;
    FOR(j,2,6)if(nd[j]==1) ++I1;
    rep(i1,5)rep(i2,6)rep(i3,6) dp[500][i1][i2][i3]=5000;
    for(;turn<M;++turn){
        if(pos==0){
            int l=0,r=0;
            while(r<N-4){
                if(V[r]==-1)++r;
                else{
                    int rg=r-l,lv=V[l];
                    cv[r]=V[r];
                    while(l<r){
                        cv[l]=(V[r]-lv)*(rg-r+l)/rg+lv;
                        ++l;
                    }
                    ++r;
                }
            }
            double t1=-INF,t2=-INF,t3=-INF,t4=-INF;
            if(timer.get()<1.8){//gcnt30 //timer<1.8sec
            for(int i=499;i>=0;--i)rep(i1,5)rep(i2,6)rep(i3,6){
                t1=-INF,t2=-INF,t3=-INF,t4=-INF;
                for(int kk=-1;kk<=1;++kk)if(i1+kk>=0&&i1+kk<5){
                    t1=max(t1,dp[i+1][i1+kk][i2][i3]);
                    t3=max(t3,dp[i+1+i2][i1+kk][i2][i3]);
                    t4=max(t4,dp[i+1+i3][i1+kk][i2][i3]);
                    t2=max(t2,dp[i+6][i1+kk][i2][i3]);
                }
                rep(k2,6){
                    t1=max(t1,dp[i+1][i1][i2][k2]);
                    t3=max(t3,dp[i+1+i2][i1][i2][k2]);
                    t4=max(t4,dp[i+1+i3][i1][i2][k2]);
                    t2=max(t2,dp[i+6][i1][i2][k2]);
                    t1=max(t1,dp[i+1][i1][k2][i3]);
                    t3=max(t3,dp[i+1+i2][i1][k2][i3]);
                    t4=max(t4,dp[i+1+i3][i1][k2][i3]);
                    t2=max(t2,dp[i+6][i1][k2][i3]);
                }
                dp[i][i1][i2][i3]=cv[i]+(t1*i1+t2*(4-i1)+t3+t4)/6-pena;
            }
            }
        }
        dec=false;
        if(N-pos<6){
            int id=turn%2;
            if(nd[id]==N-pos) id^=1;
            nd[id]=N-pos;
            dec=true;
            I1=0,I2=nd[0]-1,I3=nd[1]-1;
            FOR(j,2,6)if(nd[j]==1) ++I1;
        }
        else if(N-pos<15){
            nc[6]=0;
            rep(j,6)if(nd[j]==6) ++nc[6];
            if(nc[6]<=5){
                rep(j,6)if(nd[5-j]!=6){
                    nd[5-j]=6;
                    dec=true;
                    break;
                }
            }
            else if((N-pos)%6!=0){
                int nx=6-(N-pos)%6;
                rep(j,6)if(nc[6]==6||nd[j]!=6){
                    nd[j]=nx;
                    dec=true;
                    break;
                }
            }
            else if((N-pos)%6==0){
                rep(j,6)if(nd[j]!=6){
                    nd[j]=6;
                    break;
                }
                dec=true;
            }
            I1=0,I2=nd[0]-1,I3=nd[1]-1;
            FOR(j,2,6)if(nd[j]==1) ++I1;
        }

        if(!dec){
            double mx=-INF;
            int ni1,ni2,ni3;
            for(int kk=-1;kk<=1;++kk)if(I1+kk>=0&&I1+kk<5){
                if(mx<dp[pos][I1+kk][I2][I3]){
                    mx=dp[pos][I1+kk][I2][I3];
                    ni1=I1+kk,ni2=I2,ni3=I3;
                }
            }
            rep(kk,6){
                if(mx<dp[pos][I1][I2][kk]){
                    mx=dp[pos][I1][I2][kk];
                    ni1=I1,ni2=I2,ni3=kk;
                }
                if(mx<dp[pos][I1][kk][I3]){
                    mx=dp[pos][I1][kk][I3];
                    ni1=I1,ni2=kk,ni3=I3;
                }
            }

            nd[0]=ni2+1,nd[1]=ni3+1;
            FOR(j,2,6){
                if(j-2<ni1) nd[j]=1;
                else nd[j]=6;
            }
            I1=0,I2=nd[0]-1,I3=nd[1]-1;
            FOR(j,2,6)if(nd[j]==1) ++I1;
        }

        rep(j,6){
            cout<<nd[j]<<" "; 
        }
        cout<<endl;
        
        pre=pos;
        cin>>d>>v>>pos;
        score+=v;
        if(V[pos]==-1) V[pos]=v;
        if(pos==N) pos=0,++gcnt;
    }
    cerr<<gcnt<<" "<<score<<endl;
    return 0;
}