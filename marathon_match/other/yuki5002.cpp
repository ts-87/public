#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

unsigned long xor128(){
    static unsigned long x=123456789,y=362436069,z=521288629,w=88675123;
    unsigned long t=(x^(x<<11));
    x=y;y=z;z=w;
    return (w=(w^(w>>19))^(t^(t>>8)));
}
double timeLimit=0.92;
const int64_t CYCLES_PER_SEC=2800000000;
struct Timer {
	int64_t start;
	Timer() { reset(); }
	void reset() { start = getCycle(); }
	void plus(double a) { start -= (a * CYCLES_PER_SEC); }
	inline double get() { return (double)(getCycle() - start) / CYCLES_PER_SEC; }
	inline int64_t getCycle() {
		uint32_t low, high;
		__asm__ volatile ("rdtsc" : "=a" (low), "=d" (high));
		return ((int64_t)low) | ((int64_t)high << 32);
	}
};
int n,k;
int L[501];
string s[60];
bool g[60][60],g2[60][60];
pair<pint,pint> ans[500];
int lx,rx,ly,ry;
int best,sub;
int sumx[61][61],sumy[61][61];
pair<pint,pint> calc1(int length){
    int x,y;
    bool f;
    int best=-1000100010;
    rep(i,n)rep(j,n){
        sumx[i][j+1]=sumx[i][j]+g[i][j];
        sumy[i+1][j]=sumy[i][j]+g[i][j];
    }
    FOR(i,n-length+1,n){
        if(best==length) break;
        rep(j,n-length+1){
            if(g[i][j]){
                int score=sumx[i][j+length]-sumx[i][j];
                if(best<score){
                    x=j,y=i,f=true;
                    best=score;
                }
            }
            if(g[j][i]){
                int score=sumy[j+length][i]-sumy[j][i];
                if(best<score){
                    y=j,x=i,f=false;
                    best=score;
                }
            }
        }
    }
    for(int i=n-length;i>=0;--i){
        if(best==length) break;
        for(int j=n-length;j>=0;--j)if(g[i][j]){
            int score=sumx[i][j+length]-sumx[i][j];
            if(best<score){
                x=j,y=i,f=true;
                best=score;
            }
            score=sumy[i+length][j]-sumy[i][j];
            if(best<score){
                y=i,x=j,f=false;
                best=score;
            }
        }
    }
    if(f)FOR(j,x,x+length) g[y][j]^=1,sub+=g[y][j]?-1:1;
    else FOR(i,y,y+length) g[i][x]^=1,sub+=g[i][x]?-1:1;
    if(f) return {pint(y,x),pint(y,x+length-1)};
    else return {pint(y,x),pint(y+length-1,x)};
}
pair<pint,pint> calc2(int length){
    int x,y;
    bool f;
    int best=-1000100010;
    rep(i,n)rep(j,n){
        sumx[i][j+1]=sumx[i][j]+g[i][j];
        sumy[i+1][j]=sumy[i][j]+g[i][j];
    }
    rep(i,n-length+1){
        if(best==length) break;
        rep(j,n-length+1)if(g[i][j]){
            int score=sumx[i][j+length]-sumx[i][j];
            if(best<score){
                x=j,y=i,f=true;
                best=score;
            }
            score=sumy[i+length][j]-sumy[i][j];
            if(best<score){
                y=i,x=j,f=false;
                best=score;
            }
        }
    }
    FOR(i,n-length+1,n){
        if(best==length) break;
        rep(j,n-length+1){
            if(g[i][j]){
                int score=sumx[i][j+length]-sumx[i][j];
                if(best<score){
                    x=j,y=i,f=true;
                    best=score;
                }
            }
            if(g[j][i]){
                int score=sumy[j+length][i]-sumy[j][i];
                if(best<score){
                    y=j,x=i,f=false;
                    best=score;
                }
            }
        }
    }
    if(f)FOR(j,x,x+length) g[y][j]^=1,sub+=g[y][j]?-1:1;
    else FOR(i,y,y+length) g[i][x]^=1,sub+=g[i][x]?-1:1;
    if(f) return {pint(y,x),pint(y,x+length-1)};
    else return {pint(y,x),pint(y+length-1,x)};
}
int main(){
    Timer timer;
    timer.reset();
    cin>>n>>k;
    rep(i,k) cin>>L[i];
    rep(i,n) cin>>s[i];
    rep(i,n)rep(j,n) g[i][j]=s[i][j]-'0';
    rep(i,k) ans[i]=i%2?calc2(L[i]):calc1(L[i]);
    memcpy(g2,g,sizeof(g));
    int id,num=6;
    vector<pint> cnd(num);
    vector<pair<pint,pint> >tmp(num);
    double t;
    int cnt=0;
    //best=sub;
    while(1){
        t=timer.get();
        if(t>timeLimit) break;
        sub=0;
        rep(l,num){
            while(id=xor128()%k,count(cnd.begin(),cnd.begin()+l,pint(L[id],id))||L[id]==1);
            cnd[l]={L[id],id};
            ly=ans[id].first.first,lx=ans[id].first.second,ry=ans[id].second.first,rx=ans[id].second.second;
            if(ly==ry)FOR(j,lx,rx+1) g[ly][j]^=1,sub+=g[ly][j]?-1:1;
            else FOR(i,ly,ry+1) g[i][lx]^=1,sub+=g[i][lx]?-1:1;
        }
        sort(cnd.begin(),cnd.begin()+num,greater<pint>());
        rep(i,num){
            tmp[i]=cnt%2?calc2(cnd[i].first):calc1(cnd[i].first);
        }
        if(sub>=0){
            //best+=sub;
            memcpy(g2,g,sizeof(g));
            rep(i,num)ans[cnd[i].second]=tmp[i];
        }
        else memcpy(g,g2,sizeof(g2));
        ++cnt;
    }
    //cerr<<cnt<<" "<<best<<endl;
    rep(i,k){
        cout<<ans[i].first.first+1<<" "<<ans[i].first.second+1<<" "<<ans[i].second.first+1<<" "<<ans[i].second.second+1<<endl;
    }
    return 0;
}