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
double timeLimit=9.965;
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
vector<int> conv;
bool comp(const int& a,const int& b){
    return conv[a]<conv[b];
}
const int statesize=3;
pint score[statesize];
vector<int> state[statesize],anstate[statesize];
class MapRecoloring {
public:
    Timer timer;
    int h,w,sz,regsz,oldusedcolors;
    bool used[4096];
    bool vis[4096];
    int oldcolormap[4096][6];
    vector<int> g[4096];
    vector<int> ans;
    int mincolor,minremain;
    int bestscore;
    int worsemul;
    vector<pint> degree;   
    void init_Coloring(){
        mincolor=regsz,minremain=1000100010;
        vector<int> perm(regsz);
        rep(i,regsz) perm[i]=degree[i].second;
        vector<int> now(regsz,-1);
        int cur=0;
        now[perm[0]]=cur++;
        FOR(i,1,regsz){
            int id=perm[i];
            memset(used,0,sizeof(used));
            rep(j,g[id].size())if(now[g[id][j]]!=-1)used[now[g[id][j]]]=true;
            rep(j,cur)if(!used[j]){
                now[id]=j;
                break;
            }
            if(now[id]==-1) now[id]=cur++;
        }
        now=colorconversion(now);
        now=presolve_perm(now,cur);
        int remain=now.back();
        now.pop_back();
        score[0]={cur*remain,cur},state[0]=perm,anstate[0]=now;
        double curscore=cur*remain;
        if(bestscore>curscore){
            mincolor=cur,minremain=remain;
            ans=now;bestscore=curscore;
        }
        int swapnum=(int)sqrt(regsz)/2;
        //int swapnum=2;
        int sr=degree[0].second;
        cur=0;
        memset(vis,0,sizeof(vis));
        queue<int> q;q.push(sr);
        vis[sr]=true;
        while(!q.empty()){
            int pi=q.front();q.pop();
            perm[cur++]=pi;
            rep(i,g[pi].size()){
                int nx=g[pi][i];
                if(!vis[nx]){
                    vis[nx]=true;
                    q.push(nx);
                }
            }
        }
        FOR(k,1,statesize){
            vector<int> now(regsz,-1);
            int cur=0;
            now[perm[0]]=cur++;
            FOR(i,1,regsz){
                int id=perm[i];
                memset(used,0,sizeof(used));
                rep(j,g[id].size())if(now[g[id][j]]!=-1)used[now[g[id][j]]]=true;
                rep(j,cur)if(!used[j]){
                    now[id]=j;
                    break;
                }
                if(now[id]==-1) now[id]=cur++;
            }
            now=colorconversion(now);
            now=presolve_perm(now,cur);
            int remain=now.back();
            now.pop_back();
            score[k]={cur*remain,cur},state[k]=perm,anstate[k]=now;
            int curscore=cur*remain;
            if(bestscore>curscore){
                mincolor=cur,minremain=remain;
                ans=now;bestscore=curscore;
            }
            rep(i,swapnum) swap(perm[xor128()%regsz],perm[xor128()%regsz]);
        } 
    }
    void solve_Colering(){
        int initswapnum=(int)sqrt(regsz);
        bool flag=true;
        double sub=0,st=timer.get();
        while(1){
            double currenttime=timer.get();
            if(currenttime>timeLimit-sub) break;
            int swapnum=max(2,(int)(initswapnum*(timeLimit-currenttime)/30));
            rep(k,5){
            vector<int> now(regsz,-1);
            int stateid=xor128()%statesize;
            vector<int> perm=state[stateid];
            rep(i,swapnum) swap(perm[xor128()%regsz],perm[xor128()%regsz]);
            int cur=0;
            now[perm[0]]=cur++;
            FOR(i,1,regsz){
                int id=perm[i];
                memset(used,0,sizeof(used));
                rep(j,g[id].size())if(now[g[id][j]]!=-1)used[now[g[id][j]]]=true;
                rep(j,cur)if(!used[j]){
                    now[id]=j;
                    break;
                }
                if(now[id]==-1) now[id]=cur++;
            }
            if(score[stateid].second>=cur){
                now=colorconversion(now);
                now=presolve_perm(now,cur);
                int remain=now.back();now.pop_back();
                int curscore=cur*remain;
                if(bestscore>curscore){
                    mincolor=cur,minremain=remain;
                    ans=now;bestscore=curscore;
                }
                if(score[stateid].first>cur*remain){
                    score[stateid]={cur*remain,cur};
                    state[stateid]=perm;
                    anstate[stateid]=now;
                }
            }
            }
            if(flag){
                sub=timer.get()-st;sub*=2;
                flag=false;
            }
        }
        //cerr<<sub<<endl;
    }
    vector<int> presolve_perm(vector<int> &perm,int cur){
        vector<int> ret(regsz+1),cop(cur),best;
        int initswapnum=max(1,cur/3);
        rep(i,cur) cop[i]=i;
        int mn=1000100010;
        int loop=60;
        rep(p,loop){
            int sum=0;
            int swapnum=max(1,initswapnum*(loop-p)/loop);
            rep(i,regsz){
                if(cop[perm[i]]>oldusedcolors) sum+=oldcolormap[i][5];
                else{
                    sum+=oldcolormap[i][5]-oldcolormap[i][cop[perm[i]]];
                }
            }
            if(mn>sum){
                rep(i,regsz){
                    ret[i]=cop[perm[i]];
                }
                mn=sum;ret[regsz]=mn;best=cop;
                //swapnum=max(1,swapnum-swapnum/5-1);
            }
            else cop=best;
            rep(j,swapnum) swap(cop[xor128()%cur],cop[xor128()%cur]);
        }
        return ret;
    }
    vector<int> colorconversion(vector<int> &perm){
        vector<int> ret(regsz),conv(regsz);
        vector<pint> cocnt(regsz);
        rep(i,regsz) cocnt[i]={0,i};
        rep(i,regsz){
            cocnt[perm[i]].first+=oldcolormap[i][5];
        }
        sort(cocnt.begin(),cocnt.end(),greater<pint>());
        rep(i,regsz) conv[cocnt[i].second]=i;
        rep(i,regsz) ret[i]=conv[perm[i]];
        return ret;
    }
    vector<int> recolor(int H, vector<int> regions, vector<int> oldColors) {
        timer.reset();
        h=H;w=regions.size()/h;sz=h*w;
        regsz=*max_element(regions.begin(), regions.end())+1;
        bestscore=regsz*sz;
        rep(i,sz){
            int cur=regions[i];
            if(i%w<w-1&&cur!=regions[i+1]){
                g[cur].pb(regions[i+1]);
                g[regions[i+1]].pb(cur);
            }
            if(i/w<h-1&&cur!=regions[i+w]){
                g[cur].pb(regions[i+w]);
                g[regions[i+w]].pb(cur);
            }
            oldusedcolors=max(oldusedcolors,oldColors[i]);
            ++oldcolormap[cur][oldColors[i]];
            ++oldcolormap[cur][5];    
        }
        rep(i,regsz){
            sort(g[i].begin(),g[i].end());
            g[i].erase(unique(g[i].begin(),g[i].end()),g[i].end());
        }
        rep(i,regsz){
            degree.pb(g[i].size(),i);
        }
        sort(degree.begin(),degree.end(),greater<pint>());
        conv.resize(regsz);
        rep(i,regsz){
            conv[degree[i].second]=i;
        }
        rep(i,regsz){
            sort(g[i].begin(),g[i].end(),comp);
        }
        init_Coloring();
        solve_Colering();
        //solve_perm();
        
        cerr<<h*w<<" "<<regsz<<endl;
        return ans;
    }
};
// -------8<------- end of solution submitted to the website -------8<-------

template<class T> void getVector(vector<T>& v) {
    for (int i = 0; i < v.size(); ++i)
        cin >> v[i];
}

int main() {
    MapRecoloring mr;
    int H, S, R;
    cin >> H >> S;
    vector<int> regions(S);
    getVector(regions);
    cin >> R;
    vector<int> oldColors(R);
    getVector(oldColors);

    vector<int> ret = mr.recolor(H, regions, oldColors);
    cout << ret.size() << endl;
    for (int i = 0; i < (int)ret.size(); ++i)
        cout << ret[i] << endl;
    cout.flush();
}
