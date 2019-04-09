#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

double timeLimit=9.8;
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


struct edge{
    uint16_t id,src,dst;
    uint8_t cost,point;
    edge(uint16_t id,uint16_t src,uint16_t dst,uint8_t cost,uint8_t point):
    id(id),src(src),dst(dst),cost(cost),point(point){}
};
inline bool comp1(const edge& a,const edge& b){
    return a.cost!=b.cost?a.cost<b.cost:a.point/a.cost>b.point/b.cost;
}
inline bool comp2(const edge& a,const edge& b){
    return a.point/a.cost!=b.point/b.cost?a.point/a.cost>b.point/b.cost:a.cost<b.cost;
}
int data[1001];
struct UnionFind{
    void init(){memset(data,-1,sizeof(data));}
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
    inline int size(){
        return st.size();
    }
    inline void clear(){
        st.clear();
        fill(pos.begin(),pos.end(),-1);
    }
};

vector<int> g2[1001];
vector<pint> g[1001],g1[1001],gg[1001];
vector<vector<int> > v1,v2;
int dist[1001],dist2[1001];
int par[1001],conv[1001][1001];
bool cvid[1001];
double fac[]={0.5,1,1.5};
int tcur=0;

vector<edge> RE,EE,VR,BE,FE;

inline bool compr(const pint& a,const pint& b){
    return a.first+40*fac[tcur]-VR[a.second].point*fac[tcur]<b.first+40*fac[tcur]-VR[b.second].point*fac[tcur];
}

inline bool isterm(int i){
    return cvid[EE[i].src]||cvid[EE[i].dst];
}


#define impl(x, last) x == last ? 0 : 31 - __builtin_clz(x ^ last)
struct RadixHeap{
    int last, size_;
    vector<pint> bucket_[32];
    RadixHeap() : last(0), size_(0){}
    inline void push(int x,int val){
        ++size_, bucket_[impl(x, last)].emplace_back(x,val);
    }
    inline pint pop(bool flag=true){
        if(bucket_[0].empty()){
            int id=1;
            while(bucket_[id].empty()) ++id;
            last = min_element(bucket_[id].begin(),bucket_[id].end())->first;
            for(auto val : bucket_[id]){
                bucket_[impl(val.first, last)].push_back(val);
            }
            bucket_[id].clear();
        }
        pint res=bucket_[0].back();
        if(flag) --size_,bucket_[0].pop_back();
        return res;
    }
    pint top(){
        return pop(false);
    }
    bool empty(){
        return !size_;
    }
};

class RoadNetwork {
public:
    Timer timer;
    int NM,N,E,R;
    int bestscore=0,finalscore=0,anoscore=0; 
    vector<int> res,ans,fans,anoans;
    
    vector<int> inid,outid;
    pint terminal[1001];
    //bool used[1000001];
    vector<bool> used;
    vector<int> memo;
    int width,cap=E,basecap,swnum=10;
    inline void shuffle(){
        int l,r,p;
        if(inid.size()>0&&outid.size()>0){
            int isz=inid.size(),osz=outid.size();
            rep(i,swnum){
                l=inid[rnd.nextInt(isz)],r=outid[rnd.nextInt(osz)];
                swap(EE[l],EE[r]);
            }
        }
        else{
            rep(i,swnum){
                p=rnd.nextInt(width)+1;
                l=rnd.nextInt(cap-p),r=p+l;
                swap(EE[l],EE[r]);
            }
        }
    }
    vector<int> troute;
    vector<int> edcnt;
    void findroute(int st,int ds){
        memset(dist,0x3F,sizeof(dist));
        memset(par,-1,sizeof(par));
        troute.clear();
        //priority_queue<pint,vector<pint>,greater<pint> > pq;
        RadixHeap pq;
        dist[st]=0;
        pq.push(0,st);
        //set<int> coz;
        //rep(i,N/5) coz.insert(rnd.nextInt(N));
        while(!pq.empty()){
            pint pi=pq.pop();
            int t=pi.second;
            if(t==ds) break;
            if(dist[t]<pi.first) continue;
            for(auto it:g[t]){
                int cost=it.first;
                //if(coz.count(it.second)>0||coz.count(t)>0) cost=0;
                int e=conv[t][it.second];
                //if(edcnt[e]>0) cost=0;
                //if((rnd.nextInt()&7)&&(cvid[t]||cvid[it.second]||edcnt[e]>0)) cost=0;
                if((rnd.nextInt()&7)&&(edcnt[e]>0)) cost=0;
                else if((rnd.nextInt()&7)==0) cost*=3;
                
                if(dist[it.second]>dist[t]+cost){
                    dist[it.second]=dist[t]+cost;
                    par[it.second]=t;
                    pq.push(dist[it.second],it.second);
                }
            }
        }
        int cur=ds;
        while(cur!=st){
            troute.emplace_back(conv[cur][par[cur]]);
            cur=par[cur];
        }

    }
    UnionFind uf;
    void improve(vector<vector<int> >& v){
        int sz=v.size();
        
        int itr=0;
        BE=EE;
        vector<int> perm,bperm;
        perm.resize(sz);
        rep(lp,2){
            v=v2;
            EE=RE;
            itr=0;
            bestscore=0;
            cap=basecap;
            width=min(cap-1,E/5);
            int stnum=8,ednum=1;
            double start=timer.get(),t=start;
            int sp=0;
            rep(i,sz) perm[i]=i;
            if(lp!=0){
                rep(i,sz){
                    int l=rnd.nextInt(sz),r=rnd.nextInt(sz);
                    swap(perm[l],perm[r]);
                }
            }
            bperm=perm;
            while(1){
                t=timer.get();
                if(t-start>1.8) break;
                swnum=(stnum+(ednum-stnum)*(t-start)/1.8);
                ++itr;
                uf.init();
                int score=0,tot=0,score2=0;
                res.clear();
                
                if(cap!=E){
                    int sel=rnd.nextInt()&15,st=-1;
                    
                    bool flag=false;
                    
                    if(sp!=0&&sel==0&&itr>1){
                        st=perm[rnd.nextInt(sp)];
                        findroute(terminal[st].first,terminal[st].second);
                        flag=true;
                    }
                    
                    fill(used.begin(),used.end(),0);
                    fill(edcnt.begin(),edcnt.end(),0);
                    sp=sz;
                    rep(ii,sz){
                        int i2=perm[ii];
                        if(st!=i2){
                            rep(j,v[i2].size()){
                                if(!used[v[i2][j]]){
                                    int i=v[i2][j];
                                    if(EE[i].cost+tot<=NM){
                                        res.emplace_back(EE[i].id);
                                        tot+=EE[i].cost;
                                        score+=EE[i].point;
                                        uf.unite(EE[i].src,EE[i].dst);
                                        used[i]=true;
                                    }
                                    else if(sp==sz) sp=ii+1;
                                }
                                else edcnt[v[i2][j]]+=1;
                            }
                        }
                        else{
                            rep(j,troute.size()){
                                if(!used[troute[j]]){
                                    int i=troute[j];
                                    if(EE[i].cost+tot<=NM){
                                        res.emplace_back(EE[i].id);
                                        tot+=EE[i].cost;
                                        score+=EE[i].point;
                                        uf.unite(EE[i].src,EE[i].dst);
                                        used[i]=true;
                                    }
                                    else if(sp==sz) sp=ii+1;
                                }
                                else edcnt[troute[j]]+=1;
                            }
                        }
                        if(tot==NM) break;
                    }
                    rep(i,R)if(uf.find(VR[i].src)==uf.find(VR[i].dst)){
                        score2+=VR[i].point;
                    }
                    score*=score2;
                    if(bestscore<=score){
                        
                        
                        if(flag) {
                            v[st]=troute;
                        }
                        
                        bestscore=score;
                        ans=res;
                        bperm=perm;
                    }
                    
                    if(bestscore>0&&t-start>1.5){
                        cap=E;
                        width=E/5;
                        int cur=0;
                        fill(used.begin(),used.end(),0);
                        //memset(used,0,sizeof(used));
                        rep(ii,sz){
                            int i2=bperm[ii];
                            rep(j,v[i2].size())if(!used[v[i2][j]]){
                                int i=v[i2][j];
                                used[i]=true;
                                BE[cur++]=EE[i];
                            }
                        }
                        rep(i,E)if(!used[i]){
                            BE[cur++]=EE[i];
                        }
                        EE=BE;
                    }
                    perm=bperm;
                    rep(i,swnum){
                        int l=rnd.nextInt(sz),r=rnd.nextInt(sz);
                        swap(perm[l],perm[r]);
                    }
                }
                else{
                    inid.clear();
                    outid.clear();
                    rep(i,cap){
                        if(EE[i].cost+tot<=NM){
                            res.emplace_back(EE[i].id);
                            tot+=EE[i].cost;
                            score+=EE[i].point;
                            uf.unite(EE[i].src,EE[i].dst);
                            inid.emplace_back(i);
                        }
                        else outid.emplace_back(i);
                        if(tot==NM){
                            copy(memo.begin()+i+1,memo.end(),back_inserter(outid));
                            break;
                        }
                    }
                    //rep(i,R)if(uf.same(VR[i].src,VR[i].dst)){
                    rep(i,R)if(uf.find(VR[i].src)==uf.find(VR[i].dst)){
                        score2+=VR[i].point;
                    }
                    score*=score2;
                    if(bestscore<=score){
                        bestscore=score;
                        ans=res;
                        BE=EE;
                    }
                    EE=BE;
                    //swnum=1;
                    shuffle();
                }
                
            }
            //cerr<<itr<<" "<<bestscore<<endl;
            if(bestscore>finalscore){
                finalscore=bestscore;
                fans=ans;
                FE=BE;
            }
        }
    }

    bool usedr[1001];
    vector<int> findSolution(int iNM, int iN, int iE, vector<string> edges, int iR, vector<string> routes){
        timer.reset();
        NM=iNM,N=iN,E=iE,R=iR;
        used.resize(E);
        edcnt.resize(E);
        int t1,t2,t3,t4;
        stringstream ss;
        //rep(i,E) g[i].reserve(4096);
        rep(i,E){
            memo.pb(i);
            ss.clear();
            ss.str(edges[i]);
            ss>>t1>>t2>>t3>>t4;
            EE.emplace_back(i,t1,t2,t3,t4);
            g[t1].emplace_back(t3*(3-t4/t3/2),t2);
            g[t2].emplace_back(t3*(3-t4/t3/2),t1);
            gg[t1].emplace_back(t3,t2);
            gg[t2].emplace_back(t3,t1);
            conv[t2][t1]=i;
            conv[t1][t2]=i;
        }

        /*
        RE=EE;
        sort(EE.begin(),EE.end(),comp2);
        rep(i,E){
            t1=EE[i].src;t2=EE[i].dst;
            conv2[t2][t1]=i;
            conv2[t1][t2]=i;
        }
        */
        rep(i,R){
            ss.clear();
            ss.str(routes[i]);
            ss>>t1>>t2>>t3;
            VR.emplace_back(i,t1,t2,-1,t3);
            g2[t1].emplace_back(t2);
            cvid[t1]=true;
            cvid[t2]=true;
        }
        int nx=-1;//mn=100000;
        vset usev(N);
        int itr=0;
        double limit=2.0;
        if(N<300) limit=3.0;
        while(timer.get()<limit){
            ++itr;
            usev.clear();
            fill(used.begin(),used.end(),0);
            memset(usedr,0,sizeof(usedr));
            res.clear();
            usev.insert(rnd.nextInt(N));
            int tot=0,score=0,score2=0;
            int cnt=0;
            RadixHeap pq;
            vector<pint> sortedDist;
            while(1){
                ++cnt;
                memset(par,-1,sizeof(par));
                memset(dist,0x3F,sizeof(dist));
                memset(dist2,0x3F,sizeof(dist2));
                //priority_queue<pint,vector<pint>,greater<pint> > pq;
                
                rep(i,usev.st.size()){
                    pq.push(0,usev.st[i]);
                    dist[usev.st[i]]=0;
                    dist2[usev.st[i]]=0;
                }
                while(!pq.empty()){
                    //pint pi=pq.top();pq.pop();
                    pint pi=pq.pop();
                    int t=pi.second;
                    if(dist[t]<pi.first) continue;
                    rep(k,g[t].size()){
                        pint it=g[t][k];
                        if(dist[it.second]>dist[t]+it.first){
                            dist[it.second]=dist[t]+it.first;
                            dist2[it.second]=dist2[t]+gg[t][k].first;
                            par[it.second]=t;
                            pq.push(dist[it.second],it.second);
                        }
                    }
                }
                
                
                //mn=100000;
                nx=-1;
                int mx=-1;
                
                if(cnt<=0){
                    rep(i,R)if(!usedr[i]){
                        int cost=dist2[VR[i].src]+dist2[VR[i].dst];
                        if(mx<cost&&tot+cost<=NM){
                            mx=cost;
                            nx=i;
                        }
                    }
                }
                else{
                    sortedDist.clear();
                    rep(i,R)if(!usedr[i]){
                        int cost=dist2[VR[i].src]+dist2[VR[i].dst];
                        if(tot+cost<=NM){
                            sortedDist.pb(cost,i);
                        }
                    }
                    int ssz=sortedDist.size();
                    if(ssz>0){
                        tcur=(itr&1);
                        //nth_element(sortedDist.begin(),sortedDist.begin()+min(2,ssz-1),sortedDist.end());
                        sort(sortedDist.begin(),sortedDist.end(),compr);
                        nx=sortedDist[rnd.nextInt(min(ssz,3))].second;
                    }
                }
                
                /*
                rep(i,R)if(!usedr[i]){
                    int cost=dist[VR[i].src]+dist[VR[i].dst];
                    if(cnt<=1&&mx<cost&&tot+cost<=NM){
                        mx=cost;
                        nx=i;
                    }
                    
                    if(cnt>1&&mn>cost&&tot+cost<=NM){
                        mn=cost;
                        nx=i;
                    }
                    
                }
                */
                int eid;
                if(nx!=-1){
                    usedr[nx]=true;
                    score2+=VR[nx].point;
                    int cur=VR[nx].src;
                    while(usev.pos[cur]==-1){
                        usev.insert(cur);
                        eid=conv[cur][par[cur]];
                        tot+=EE[eid].cost;
                        score+=EE[eid].point;
                        res.pb(EE[eid].id);
                        used[eid]=true;
                        cur=par[cur];
                    }
                    cur=VR[nx].dst;
                    while(usev.pos[cur]==-1){
                        usev.insert(cur);
                        eid=conv[cur][par[cur]];
                        tot+=EE[eid].cost;
                        score+=EE[eid].point;
                        res.pb(EE[eid].id);
                        used[eid]=true;
                        cur=par[cur];
                    }

                    if(bestscore<score*score2){
                        bestscore=score*score2;
                        fans=res;
                    }
                }
                else{
                    /*
                    rep(i,E)if(!used[i]){
                        if(EE[i].cost+tot<=NM){
                            res.pb(EE[i].id);
                            tot+=EE[i].cost;
                            score+=EE[i].point;
                        }
                    }
                    */
                    break;
                }
            }
            //cerr<<score<<" "<<score2<<" "<<score*score2<<endl;
            if(bestscore<score*score2){
                bestscore=score*score2;
                fans=res;
            }
        }
        anoans=fans;
        anoscore=bestscore;
        //cerr<<itr<<" "<<bestscore<<endl;

        //finalscore=bestscore;
        rep(i,fans.size()){
            edge& ed=EE[fans[i]];
            g1[ed.src].pb(ed.cost,ed.dst);
            g1[ed.dst].pb(ed.cost,ed.src);
            ++edcnt[fans[i]];
        }


        rep(i,N)if(g2[i].size()>0){
            
            if(g1[i].size()>0){
            memset(par,-1,sizeof(par));
            memset(dist,0x3F,sizeof(dist));
            priority_queue<pint,vector<pint>,greater<pint> > pq;
            dist[i]=0;
            pq.push({0,i});
            while(!pq.empty()){
                pint pi=pq.top();pq.pop();
                int t=pi.second;
                if(dist[t]<pi.first) continue;
                for(auto it:g1[t]){
                    //int cost=(cvid[t]||cvid[it.second])?0:it.first;
                    int cost=it.first;
                    if(dist[it.second]>dist[t]+cost){
                        dist[it.second]=dist[t]+cost;
                        par[it.second]=t;
                        pq.push({dist[it.second],it.second});
                    }
                }
            }
            rep(j,g2[i].size()){
                int cur=g2[i][j],ini=cur;
                if(g1[cur].size()==0) continue;
                vector<int> tv;
                while(cur!=i){
                    tv.emplace_back(conv[cur][par[cur]]);
                    cur=par[cur];
                }
                terminal[v1.size()]={cur,ini};
                v1.emplace_back(tv);
            }
            
            }
            
            //else{
                memset(par,-1,sizeof(par));
            memset(dist,0x3F,sizeof(dist));
            priority_queue<pint,vector<pint>,greater<pint> > pq;
            dist[i]=0;
            pq.push({0,i});
            while(!pq.empty()){
                pint pi=pq.top();pq.pop();
                int t=pi.second;
                if(dist[t]<pi.first) continue;
                for(auto it:g[t]){
                    //int cost=(cvid[t]||cvid[it.second])?0:it.first;
                    int cost=(edcnt[conv[t][it.second]]>0)?0:it.first;
                    if(dist[it.second]>dist[t]+cost){
                        dist[it.second]=dist[t]+cost;
                        par[it.second]=t;
                        pq.push({dist[it.second],it.second});
                    }
                }
            }
            rep(j,g2[i].size()){
                int cur=g2[i][j],ini=cur;
                vector<int> tv;
                while(cur!=i){
                    tv.emplace_back(conv[cur][par[cur]]);
                    cur=par[cur];
                }
                terminal[v1.size()]={cur,ini};
                v1.emplace_back(tv);
            }
            //}

        }

        v2=v1;
        RE=EE;
        improve(v1);

        itr=0;
        EE=FE;
        cap=E;
        width=E/5;
        swnum=1;
        int swl=0,swr=0,swp;
        while(timer.get()<timeLimit){
            ++itr;
            uf.init();
            
            int score=0,tot=0;
            res.clear();
            inid.clear();
            outid.clear();
            rep(i,cap){
                if(EE[i].cost+tot<=NM){
                    res.emplace_back(EE[i].id);
                    tot+=EE[i].cost;
                    score+=EE[i].point;
                    uf.unite(EE[i].src,EE[i].dst);
                    inid.emplace_back(i);
                }
                else{
                    //outid.emplace_back(i);
                    
                    if(itr&1)outid.emplace_back(i);
                    else{
                        if(cvid[EE[i].src]||cvid[EE[i].dst]){
                            outid.emplace_back(i);
                        }
                    }
                    
                }
                /*
                if(tot==NM){
                    copy(memo.begin()+i+1,memo.end(),back_inserter(outid));
                    break;
                }
                */
                
                if(tot==NM){
                    if(itr&1)copy(memo.begin()+i+1,memo.end(),back_inserter(outid));
                    else copy_if(memo.begin()+i+1,memo.end(),back_inserter(outid),isterm);
                    break;
                }
                
                
            }
            int score2=0;
            rep(i,R)if(uf.find(VR[i].src)==uf.find(VR[i].dst)){
                score2+=VR[i].point;
            }
            score*=score2;
            if(finalscore<=score){
                finalscore=score;
                fans=res;
            }
            else{
                swap(EE[swl],EE[swr]);
            }
            //EE=FE;
            int isz=inid.size(),osz=outid.size();
            if(isz>0&&osz>0){
                rep(i,10){
                    swl=inid[rnd.nextInt(isz)],swr=outid[rnd.nextInt(osz)];
                    if(EE[swr].cost<=EE[swl].cost&&EE[swr].point>=EE[swl].point) break;
                }
                swap(EE[swr],EE[swl]);
            }
            else{
                swp=rnd.nextInt(width)+1;
                swl=rnd.nextInt(cap-swp),swr=swp+swl;
                swap(EE[swl],EE[swr]);
            }
            
            
            //shuffle();
        }

        //cerr<<itr<<" "<<finalscore<<endl;
        if(finalscore<anoscore){
            fans=anoans;
        }
        return fans;
        
         
    }   
};

// -------8<------- end of solution submitted to the website -------8<-------

template<class T> void getVector(vector<T>& v, int num) {
    T a;
    std::getline(std::cin, a);      //skip the first one
    for (int i = 0; i < num; ++i)
    {
        std::getline(std::cin, a);
        v.push_back(a);
    }
}

int main() {
    RoadNetwork rn;
    int NM;
    int N;
    int E;
    int R;
    cin >> NM;
    cin >> N;
    cin >> E;
    vector<string> edges;
    getVector(edges,E);  
    cin >> R;
    vector<string> routes;
    getVector(routes,R);

    vector<int> ret = rn.findSolution(NM,N,E,edges,R,routes);
    cout << ret.size() << endl;
    for (int i = 0; i < (int)ret.size(); i++)
        cout << ret[i] << endl;
    cout.flush();
}