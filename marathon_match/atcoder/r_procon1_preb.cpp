#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

struct Rand2 {
	uint32_t y;
	static constexpr double iDouble=1.0/(1LL<<32);
	Rand2() {
		y=2463534242;
	}
	void setseed(uint32_t seed){y=seed;}
	uint32_t nextInt() {
    	return y^=(y^=(y^=y<<13)>>17)<<5;
	}
	uint32_t nextInt(uint32_t n) {
		return nextInt()%n;
	}
	double nextDouble() {return nextInt()*iDouble;}
} rnd;
const int dx[]={-1,0,1,0},dy[]={0,-1,0,1};
string dir[]={"L","U","R","D"};
int N,M;
int g[52][52],ans[52][52];
pint out[501];
int bestscore=0;
int main(){
    memset(g,-1,sizeof(g));
    memset(ans,-1,sizeof(ans));
    cin>>N>>M;
    rep(i,N)rep(j,N) cin>>g[i+1][j+1];
    ans[1][1]=0,ans[1][N]=1,ans[N][1]=2,ans[N][N]=3;
    int score=0;
    vector<pair<int,pint> > tv;
    rep(i,M){
        int mx=-1,mx2=-1;
        tv.clear();
        int co=i%4;
        rep(r,N){
            rep(c,N){
            int tc=c+1,tr=r+1;
            if(ans[tr][tc]!=co) continue;
            rep(k,4){
                int pr=dy[k],pc=dx[k];
                int pp=0;
                tc=c+1,tr=r+1;
                rep(k2,5){
                    tr+=pr,tc+=pc;
                    if(g[tr][tc]==-1) break;
                    if(g[tr][tc]==co&&ans[tr][tc]!=co) ++pp;
                    if(ans[tr][tc]==-1) pp+=2;
                    if(g[tr][tc]==co&&ans[tr][tc]==-1)pp+=100;
                }
                tv.pb(-pp,pint(r*N+c,k));
            }
            }
        }
        int sz=tv.size(),tg;
        if(sz==0){
            out[i]={-1,-1};
            continue;
        }
        else{
            sort(tv.begin(),tv.end());
            if(tv[0].first<=50) tg=rnd.nextInt(min(3,sz));
            else {
                if(rnd.nextInt()&1)tg=rnd.nextInt(min(3,sz));
                else tg=rnd.nextInt(max(1,min(3,sz/2-3)))+sz/2;
            }
            out[i]=tv[tg].second;
            mx=-tv[tg].first;
        }
        int tr=out[i].first/N+1,tc=out[i].first%N+1,pd=out[i].second;
        int pr=dy[pd],pc=dx[pd];
        rep(k2,5){
            tr+=pr,tc+=pc;
            if(g[tr][tc]==-1) break;
            ans[tr][tc]=co;
        }
        score+=mx;
    }
    cerr<<score<<endl;
    rep(i,M){
        if(out[i].first==-1) cout<<-1<<endl;
        else cout<<out[i].first/N<<" "<<out[i].first%N<<" "<<dir[out[i].second]<<endl;
    }
    return 0;
}