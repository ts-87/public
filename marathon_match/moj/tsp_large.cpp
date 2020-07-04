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

struct Rand2 {
	uint32_t y;
	static constexpr double iDouble=1.0/(1LL<<32);
	Rand2() {
		y=2463534242;
	}
	void setseed(uint32_t seed){y^=seed;}
	uint32_t nextInt() {
    	return y^=(y^=(y^=y<<13)>>17)<<5;
	}
	uint32_t nextInt(uint32_t n) {
		return nextInt()%n;
	}
	double nextDouble() {return nextInt()*iDouble;}
} rnd;

const double DINF=1e15;
const int D=16;
const int C=(1LL<<15)/D;
int N;
double X[100000],Y[100000];
bool used[100000];
vector<int> dv[D*D],perm[D*D];
pint nprg[D*D];
double greedy(int r,int preid){
    double ret=0;
    int n=dv[r].size();
    double mn,td;
    int nx,pre=-1;
    if(preid>=0){
        int vi=perm[preid].back();
        mn=DINF;
        rep(i,n){
            td=hypot(X[vi]-X[dv[r][i]],Y[vi]-Y[dv[r][i]]);
            if(mn>td) mn=td,nx=dv[r][i];
        }
        ret+=mn;perm[r].pb(nx);
        used[nx]=true;
        pre=nx;
    }
    else{
        mn=0;
        rep(i,n)if(mn<Y[dv[r][i]]) mn=Y[dv[r][i]],nx=dv[r][i];
        perm[r].pb(nx);
        used[nx]=true;pre=nx;
    }
    FOR(k,1,n){
        mn=DINF;
        rep(i,n)if(!used[dv[r][i]]){
            td=hypot(X[pre]-X[dv[r][i]],Y[pre]-Y[dv[r][i]]);
            if(mn>td) mn=td,nx=dv[r][i];
        }
        pre=nx;
        perm[r].pb(nx);
        used[nx]=true;
        ret+=mn;
    }
    if(r==D){
        ret+=hypot(X[perm[0][0]]-X[perm[r].back()],Y[perm[0][0]]-Y[perm[r].back()]);
    }
    return ret;
}
vector<int> tperm,bperm;
vector<int> neighbor[100000];
vector<pint> sted;
int conv[100000],conv2[100000];
double dist[500][500];
double LN[65536];
void improvelocal(int r){
    
    int niter=100000,n=perm[r].size();
    int pre=perm[nprg[r].first].back(),nx=perm[nprg[r].second][0];

    int cid=0;
    conv2[pre]=cid++,conv2[nx]=cid++;
    rep(i,n) conv2[perm[r][i]]=cid++;

    int nlist=15;
    rep(i,n){
        sted.clear();
        double td;
        rep(j,n)if(j!=i){
            td=hypot(X[perm[r][i]]-X[perm[r][j]],Y[perm[r][i]]-Y[perm[r][j]]);
            sted.pb(td,perm[r][j]);
            dist[conv2[perm[r][i]]][conv2[perm[r][j]]]=td;
        }
        dist[conv2[perm[r][i]]][conv2[pre]]=hypot(X[perm[r][i]]-X[pre],Y[perm[r][i]]-Y[pre]);
        dist[conv2[perm[r][i]]][conv2[nx]]=hypot(X[perm[r][i]]-X[nx],Y[perm[r][i]]-Y[nx]);
        nth_element(sted.begin(),sted.begin()+nlist+1,sted.end());
        rep(k,nlist) neighbor[perm[r][i]].pb(sted[k].second);
    }
    
    double lcost=0;
    tperm.clear();tperm.pb(pre);
    lcost+=hypot(X[pre]-X[perm[r][0]],Y[pre]-Y[perm[r][0]]);
    tperm.pb(perm[r][0]);
    conv[perm[r][0]]=1;
    FOR(i,1,n){
        tperm.pb(perm[r][i]);
        //lcost+=hypot(X[perm[r][i]]-X[perm[r][i-1]],Y[perm[r][i]]-Y[perm[r][i-1]]);
        lcost+=dist[conv2[perm[r][i]]][conv2[perm[r][i-1]]];
        conv[perm[r][i]]=i+1;
    }
    tperm.pb(nx);
    lcost+=hypot(X[nx]-X[perm[r][n-1]],Y[nx]-Y[perm[r][n-1]]);
    double bestcost=lcost,diff;
    double nco,pco;
    double ts,starttemp=35,endtemp=1;
    bperm=tperm;
    rep(itr,niter){
        ts=starttemp+(endtemp-starttemp)*itr/niter;
        int id1,id2;
        
        id1=itr%n+1;//rnd.nextInt(n)+1;
        id2=neighbor[tperm[id1]][rnd.nextInt(nlist)];
        id2=conv[id2];
        //id2=rnd.nextInt(n)+1;
        
        if(id1>id2) swap(id1,id2);
        pco=dist[conv2[tperm[id1]]][conv2[tperm[id1-1]]]
            +dist[conv2[tperm[id2]]][conv2[tperm[id2+1]]];
        nco=dist[conv2[tperm[id2]]][conv2[tperm[id1-1]]]
            +dist[conv2[tperm[id1]]][conv2[tperm[id2+1]]];
        //pco=hypot(X[tperm[id1-1]]-X[tperm[id1]],Y[tperm[id1-1]]-Y[tperm[id1]]);
        //pco+=hypot(X[tperm[id2]]-X[tperm[id2+1]],Y[tperm[id2]]-Y[tperm[id2+1]]);
        //nco=hypot(X[tperm[id1-1]]-X[tperm[id2]],Y[tperm[id1-1]]-Y[tperm[id2]]);
        //nco+=hypot(X[tperm[id1]]-X[tperm[id2+1]],Y[tperm[id1]]-Y[tperm[id2+1]]);
        diff=pco-nco;
        //if(diff>=0){
        if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
            lcost-=diff;
            //bestcost=lcost;
            //reverse(tperm.begin()+id1,tperm.begin()+id2+1);
            int rr=(id2-id1+1)/2+id1;
            FOR(k,id1,rr){
                swap(tperm[k],tperm[id2-k+id1]);
                swap(conv[tperm[k]],conv[tperm[id2-k+id1]]);
            }
            if(bestcost>lcost){
                bestcost=lcost;
                bperm=tperm;
            }
        }
        
    }
    FOR(i,1,n+1){
        perm[r][i-1]=bperm[i];
    }
}
int main(){
    ios::sync_with_stdio(false),cin.tie(0),cout.tie(0);
    Timer timer;
    double tmpm=1.0/(2.0*65536);
    rep(i,65536){
        LN[i]=log((double)i/65536+tmpm);
    }
    cin>>N;
    rep(i,N){
        cin>>X[i]>>Y[i];
        dv[(int)Y[i]/C*D+(int)X[i]/C].pb(i);
    }
    double cost=0;
    int gi=0,gp=-1;
    rep(ii,D*D){
        cost+=greedy(gi,gp);
        gp=gi;
        if(gi==0) ++gi;
        else if(gi%D==0) gi-=D;
        else if(gi/D%2==0){
            if(gi%D==D-1) gi+=D;
            else ++gi;
        }
        else{
            if(gi%D==1){
                if(gi/D==D-1) --gi;
                else gi+=D;
            }
            else --gi;
        }
        nprg[gp].second=gi;
        nprg[gi].first=gp;
    }
    cerr<<setprecision(2)<<fixed<<cost<<endl;

    int cur=0,cnt=0;
    while(1){
        improvelocal(cur);
        cur=nprg[cur].second;
        if(cur==0) break;
    }
    cur=0;
    cost=0;
    while(1){
        int sz=perm[cur].size();
        int pre=nprg[cur].first;
        cost+=hypot(X[perm[cur][0]]-X[perm[pre].back()],Y[perm[cur][0]]-Y[perm[pre].back()]);
        cout<<perm[cur][0]<<"\n";
        FOR(i,1,sz){
            cost+=hypot(X[perm[cur][i]]-X[perm[cur][i-1]],Y[perm[cur][i]]-Y[perm[cur][i-1]]);
            cout<<perm[cur][i]<<"\n";
        }
        cur=nprg[cur].second;
        if(cur==0) break;
    }
    cout<<endl;
    cerr<<setprecision(2)<<fixed<<cost<<endl;
    cerr<<timer.get()<<endl;
    return 0;
}