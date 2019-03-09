//Hanan Grid
#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

double timeLimit=9.97;
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
struct Rand {
	uint32_t x,y,z,w;
	static constexpr double iDouble=1.0/(1LL<<32);
	Rand() {
		x=123456789;y=362436069;z=521288629;w=88675123;
	}
	void setseed(uint32_t seed){z^=seed;}
	uint32_t nextInt(uint32_t n) {
		uint32_t t=x^(x<<11);
		x=y;y=z;z=w;
		w=(w^(w>>19))^(t^(t>>8));
		return w%n;
	}
	uint32_t nextInt() {
		uint32_t t=x^(x<<11);
		x=y;y=z;z=w;
		return w=(w^(w>>19))^(t^(t>>8));
	}
	double nextDouble() {return nextInt()*iDouble;}
} rnd;

struct vset{
    vector<int> st,pos;
    vset(int n){
        pos.resize(n,-1);
        st.reserve(n);
    }
    inline void insert(int a){
        if(pos[a]==-1){
            pos[a]=st.size();
            st.push_back(a);
        }
    }
    inline void erase(int a){
        if(pos[a]!=-1){
            swap(st[pos[a]],st.back());
            pos[st[pos[a]]]=pos[a];
            st.pop_back();
            pos[a]=-1;
        }
    }
};
struct edge{
    int src,dst,cost;
    edge(){};
    edge(int src,int dst,int cost):
    src(src),dst(dst),cost(cost){}
};
bool operator < (const edge &m,const edge &f){
    return m.cost!=f.cost?m.cost<f.cost:
    m.src!=f.src?m.src<f.src:m.dst<f.dst;
}


int data[10002]; 
struct UnionFind{
    void init(int n){memset(data,-1,sizeof(data));}
    int find(int x){return data[x]<0?x:data[x]=find(data[x]);}
    void unite(int x, int y){
        x=find(x);y=find(y);       
        if(x!=y){
            if(data[y]<data[x]) swap(x,y);
            data[x]+=data[y];data[y]=x;
        }
        return;
    } 
    bool same(int x, int y){return (find(x) == find(y));}
    int size(int x){return -data[find(x)];}
};

const int W=100;
Timer timer;
int N,M,MXM;
int base[10001];
vset spo(10001),cand(10001);
vector<int> bestpo;
vector<edge> inite,be,bbe,fbe,tbe,bestbe;
int mxedge=0;
int bestscore=0;

int initkrus(){
    sort(inite.begin(),inite.end());
    UnionFind uf;
    uf.init(10001);
    double res=0;
    rep(i,inite.size()){
        edge m=inite[i];
        if(!uf.same(m.src,m.dst)){
            uf.unite(m.src,m.dst);
            be.emplace_back(edge(m.src,m.dst,m.cost));
            res+=m.cost-1;
            mxedge=max(mxedge,m.cost);
        }
    }
    return res;
}
int kruskal(bool flag=false){
    tbe=be;
    sort(tbe.begin(),tbe.end());
    UnionFind uf;
    uf.init(10001);
    int res=0;
    rep(i,tbe.size()){
        edge m=tbe[i];
        if(!uf.same(m.src,m.dst)){
            uf.unite(m.src,m.dst);
            if(flag)fbe.emplace_back(edge(m.src,m.dst,m.cost));
            res+=m.cost-1;
        }
    }
    if(M==0) return res+1;
    else return res+M;
}


inline bool forceupdate(double diff,double temp){
    if(diff>=0) return true;
    double d=(diff)*temp;
    if(d<-6) return false;
    return exp(d)>rnd.nextDouble();
}
void improve(){
    double start=timer.get();
    int turn=0;
    int diff;
    double t=start,invtl=1.0/timeLimit;
    int curscore=bestscore,prescore;
    double starttemp=0.5,endtemp=0.1,curtemp=1.0/starttemp;
    while(1){
        ++turn;
        if((turn&15)==0){
            t=timer.get();
            curtemp=1.0/(starttemp+(endtemp-starttemp)*(t-start)*invtl);
        }
        if(t>timeLimit){
            be=bestbe;
            cerr<<turn<<endl;
            return;
        }
        int sel=rnd.nextInt()&1;
        
        prescore=curscore;
        if((sel==0&&M!=0)||MXM==M){
            //delete
            be=bbe;
            int id=rnd.nextInt(M)+N;
            int pos=spo.st[id];
            spo.erase(pos);
            cand.insert(pos);
            --M;
            FOR(i,0,N)FOR(j,N,N+M){
                int co=abs(spo.st[i]%W-spo.st[j]%W)+abs(spo.st[i]/W-spo.st[j]/W);
                if(co<=mxedge)be.pb(edge(spo.st[i],spo.st[j],co));
            }
            FOR(i,N,N+M)FOR(j,i+1,N+M){
                int co=abs(spo.st[i]%W-spo.st[j]%W)+abs(spo.st[i]/W-spo.st[j]/W);
                if(co<=mxedge)be.pb(edge(spo.st[i],spo.st[j],co));
            }
            curscore=kruskal();
            
            diff=prescore-curscore;
            if(forceupdate(diff,curtemp)){
                
                if(bestscore>curscore){
                    bestscore=curscore;
                    bestbe=be;
                    bestpo=spo.st;
                }
            }
            else{
                curscore=prescore;
                spo.insert(pos);
                cand.erase(pos);
                ++M;
            }
        }
        else if(sel==1||M==0){
            //add
            be=bbe;
            int pos;
            while(1){
                pos=cand.st[rnd.nextInt(cand.st.size())];
                if(spo.pos[pos]==-1) break;
            }
            spo.insert(pos);
            cand.erase(pos);
            ++M;
            FOR(i,0,N)FOR(j,N,N+M){
                int co=abs(spo.st[i]%W-spo.st[j]%W)+abs(spo.st[i]/W-spo.st[j]/W);
                if(co<=mxedge)be.pb(edge(spo.st[i],spo.st[j],co));
            }
            FOR(i,N,N+M)FOR(j,i+1,N+M){
                int co=abs(spo.st[i]%W-spo.st[j]%W)+abs(spo.st[i]/W-spo.st[j]/W);
                if(co<=mxedge)be.pb(edge(spo.st[i],spo.st[j],co));
            }
            curscore=kruskal();
            diff=prescore-curscore;
            if(forceupdate(diff,curtemp)){
                
                if(bestscore>curscore){
                    bestscore=curscore;
                    bestbe=be;
                    bestpo=spo.st;
                }
            }
            else{
                curscore=prescore;
                spo.erase(pos);
                cand.insert(pos);
                --M;
            }
        }
    }
}
int cntx[100],cnty[100];
int main(){
    timer.reset();
    cin>>N;
    int xi,yi;
    rep(i,N){
        cin>>xi>>yi;
        base[yi*W+xi]=true;
        spo.insert(yi*W+xi);
        ++cntx[xi],++cnty[yi];
    }
    rep(i,N)FOR(j,i+1,N){
        int tcost=abs(spo.st[i]/W-spo.st[j]/W)+abs(spo.st[i]%W-spo.st[j]%W);
        inite.emplace_back(edge(spo.st[i],spo.st[j],tcost));
    }
    bestscore=initkrus();
    bbe=be;

    rep(i,W)rep(j,W)if(!base[i*W+j]){
        if(cntx[j]>0&&cnty[i]>0) cand.insert(i*W+j);
    }
    MXM=cand.st.size();

    bestbe=be;
    improve();
    be=bestbe;

    kruskal(true);
    
    cerr<<bestscore<<endl;

    set<pint> addPanel;
    rep(i,fbe.size()){
        edge m=fbe[i];
        int fx=m.src%W,tx=m.dst%W,fy=m.src/W,ty=m.dst/W;
        int mv=(fx<=tx)?1:-1;
        while(fx!=tx){
            fx+=mv;
            addPanel.insert({fx,fy});
        }
        mv=(fy<=ty)?1:-1;
        while(fy!=ty){
            fy+=mv;
            addPanel.insert({fx,fy});
        }
    }
    rep(i,bestpo.size()){
        addPanel.insert({bestpo[i]%W,bestpo[i]/W});
    }
    vector<pint> ans;
    for(auto it:addPanel){
        if(!base[it.second*W+it.first]) ans.emplace_back(it);
    }
    cout<<ans.size()<<endl;
    for(auto it:ans){
        cout<<it.first<<" "<<it.second<<endl;
    }
    return 0;
}