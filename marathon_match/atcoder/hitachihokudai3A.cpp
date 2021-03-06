#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

int N,K,M,initK;
int D[4096],C[4096];
vector<int> V[4096];
vector<int> A1,A2,bA1,bA2;
vector<int> AD,AC,bAD,bAC;
int qcf[4097][4097],scf[4097];
int mxcf=0,tom=0,tl=0;
ll curscore=0,bestscore=0;
set<int> sp,bsp;
int limit=750,dlim=5;
int bestnewvar=0;
inline double pdscore(int eSA,int m,int l,int mc){
    return 10000.0*10000*(1.0-min(eSA,100)*1.0/100)*1000/(5*m+l+1000)*1000/(mc+1000);
}
ll msk[4097];
uint32_t Hash[4097];
uint32_t H[65535];


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

int varCount=0;

int constant=0;

bool reducted[65535];
unordered_map<uint32_t,int> hdic;
int mxd=0;

void splitPos(){
    //min(750,cfmx);
    int cur=K;
    rep(i,cur)if(D[i]>2&&C[i]>0&&!reducted[i]&&abs(C[i]*(D[i]-2))>limit&&bsp.count(i)==0){//bsp.count(i)==0
        int mid=D[i]/2;

        D[cur]=mid;C[cur]=C[i];
        rep(j,mid){
            V[cur].pb(V[i][j]);
            H[cur]^=Hash[V[i][j]];
        }
        H[cur]^=Hash[varCount];
        hdic[H[cur]]=cur;
        V[cur].pb(varCount);

        ++cur;

        uint32_t tmp=0;
        FOR(j,mid,D[i]){
            tmp^=Hash[V[i][j]];
        }
        int id=hdic[tmp];
        if(id!=0&&!reducted[id]){
            int id=hdic[tmp];
            C[id]+=C[i];
        }
        else{
            D[cur]=D[i]-mid;C[cur]=C[i];
            H[cur]=tmp;hdic[H[cur]]=cur;
            FOR(j,mid,D[i]){
                V[cur].pb(V[i][j]);
            }

            ++cur;
        }

        D[cur]=D[i]-mid+1;C[cur]=-C[i];
        FOR(j,mid,D[i]){
            V[cur].pb(V[i][j]);
            H[cur]^=Hash[V[i][j]];
        }
        V[cur].pb(varCount);hdic[H[cur]]=cur;
        H[cur]^=Hash[varCount];

        ++cur;

        ++varCount;
        reducted[i]=true;
    }
    K=cur;
}
void splitNeg(){
    //min(750,cfmx);
    int cur=K;
    rep(i,cur)if(D[i]>2&&C[i]<0&&!reducted[i]&&abs(C[i]*(D[i]-1))>limit){
        int mid=D[i]/2;

        D[cur]=1;C[cur]=-C[i];
        V[cur].pb(varCount);
        H[cur]=Hash[varCount];hdic[H[cur]]=cur;

        ++cur;

        D[cur]=mid+1;C[cur]=C[i];
        rep(j,mid){
            V[cur].pb(V[i][j]);
            H[cur]^=Hash[V[i][j]];
        }
        V[cur].pb(varCount);
        H[cur]^=Hash[varCount];hdic[H[cur]]=cur;

        ++cur;

        D[cur]=D[i]-mid+1;C[cur]=C[i];
        FOR(j,mid,D[i]){
            V[cur].pb(V[i][j]);
            H[cur]^=Hash[V[i][j]];
        }
        V[cur].pb(varCount);hdic[H[cur]]=cur;
        H[cur]^=Hash[varCount];

        ++cur;
        ++varCount;
        reducted[i]=true;
    }
    K=cur;
}
void setoff(){
    rep(i,K)if(!reducted[i]){
        if(C[i]>0&&D[i]>=dlim){
            int mx=dlim-1,id=-1;
            rep(j,K){
                if(C[j]+C[i]<=0&&(msk[i]&msk[j])==msk[j]&&mx<D[j]&&!reducted[j]){
                    mx=max(mx,D[j]);
                    id=j;
                }
            }
            if(id!=-1){
                reducted[i]=true;
                C[id]+=C[i];
                if(C[id]==0) reducted[id]=true;
            }
        }
    }
    rep(i,K)if(!reducted[i]){
        if(C[i]>0&&D[i]>=dlim){
            int mx=dlim-1,id=-1;
            rep(j,K){
                if(C[j]<0&&C[i]+C[j]>=0&&(msk[i]&msk[j])==msk[j]&&mx<D[j]&&!reducted[j]){
                    mx=max(mx,D[j]);
                    id=j;
                }
            }
            if(id!=-1){
                reducted[id]=true;
                C[i]+=C[id];
                if(C[i]==0) reducted[id]=true;
                --i;
            }
        }
    }
}
set<int> st;
void comsplit(){
    int cur=K;
    rep(i,K)if(!reducted[i]&&C[i]>0&&D[i]>=dlim){
        int tmpcf=C[i];
        ll tmpmsk=msk[i];
        st.clear();
        st.insert(i);
        while(1){
            bool update=false;
            int nxid=-1,mx=dlim-1;
            rep(j,K)if(!reducted[j]&&C[j]>0&&i!=j&&abs(tmpcf+C[j])*(dlim-2)<mxcf&&st.count(j)==0){
                int ct=__builtin_popcountll(tmpmsk&msk[j]);
                if(mx<ct){
                    mx=ct;
                    nxid=j;
                    update=true;
                }
            }
            if(!update) break;
            else{
                st.insert(nxid);
                tmpmsk&=msk[nxid];
                tmpcf+=C[nxid];
            }
        }
        if(st.size()>1){
            for(auto it:st){
                reducted[it]=true;
            }
            D[cur]=dlim;C[cur]=tmpcf;
            int cnt=0;
            rep(j,N){
                if(tmpmsk>>j&1ll){
                    V[cur].pb(j);
                    H[cur]^=Hash[j];
                    ++cnt;
                }
                if(cnt==dlim) break;
            }
            hdic[H[cur]]=cur;
            ++cur;
        }
        
    }
    K=cur;
}
ll curstate,beststate;
ll foolish(){
    ll bscr=0,curscr=0,prescr=0;
    int turn=0;
    int mcf=0,ntm=1,nv=0;
    while(1){
        ++turn;
        if(turn==10000){
            //cerr<<nv<<" "<<ntm<<" "<<mcf<<endl;
            break;
        }

        mcf=0,ntm=1,nv=0;
        int sm=0;
        int id;
        if(turn!=1){
            id=rnd.nextInt(N);
            curstate^=(1ll<<id);
        }
        prescr=curscr;
        rep(i,K){
            if(D[i]>0&&C[i]>0){
                if(__builtin_popcountll(msk[i]&curstate)!=0){
                    sm+=C[i];
                }
            }
            else if(D[i]>2&&C[i]<0){
                if(__builtin_popcountll(msk[i]&curstate)!=0){
                    sm+=C[i];
                    ++ntm;
                    mcf=max(mcf,-C[i]);
                }
                else{
                    ntm+=(D[i]+1);
                    mcf=max(mcf,-(D[i]-1)*C[i]);
                    ++nv;
                }
            }
            else{
                ++ntm;
                mcf=max(mcf,-C[i]);
            }
        }
        if(sm>=0){
            curscr=pdscore(0,nv,ntm,mcf)/2;
            if(prescr<=curscr){
                prescr=curscr;
            }
            else{
                curstate^=(1ll<<id);
            }
            if(bscr<curscr){
                beststate=curstate;
                bscr=curscr;
            }
            
        }
        else if(prescr!=0){
            curstate^=(1ll<<id);
        }
    }
    return bscr;
}
int tD[65535],tC[65535];
uint32_t tH[65535];
vector<int> tV[65535];
pint sorted[1025];
bool used[4096];
int actvar[4096];
ll tmsk[4096];
int main(){
    cin>>N>>K;
    initK=K;
    rep(i,4096){
        Hash[i]=rnd.nextInt();
        //Hash[i]=xor64();
    }
    rep(i,K){
        int vi;
        cin>>tD[i]>>tC[i];
        rep(j,tD[i]){
            cin>>vi;
            --vi;
            tV[i].pb(vi);
            tH[i]^=Hash[vi];
            tmsk[i]|=(1ll<<vi);
        }
        if(tD[i]>2){
            if(tC[i]>0) mxcf=max(mxcf,abs(tC[i]*min(dlim-2,tD[i]-2)));
            else mxcf=max(mxcf,abs(tC[i]*(tD[i]-1)));
        }
        else mxcf=max(mxcf,abs(tC[i]));
        mxd=max(mxd,tD[i]);
        sorted[i]={tD[i],i};
    }
    sort(sorted,sorted+K,greater<pint>());
    rep(i,K){
        int id=sorted[i].second;
        D[i]=tD[id],C[i]=tC[id];
        V[i]=tV[id];H[i]=tH[id];
        msk[i]=tmsk[id];
        hdic[H[i]]=i;

    }
    mxcf=min(limit,mxcf);
    
    setoff();
    comsplit();

    rep(i,K)if(!reducted[i]&&C[i]>0&&D[i]>dlim){
        D[i]=dlim;
        V[i].resize(dlim);
    }
    
    varCount=N;
    splitPos();
    splitNeg();
    rep(i,K)if(!reducted[i]){
        if(D[i]>2){
            if(C[i]<0){
                scf[varCount]+=-C[i]*(D[i]-1);
                rep(j,V[i].size()){
                    qcf[V[i][j]][varCount]+=C[i];
                }
                ++varCount;
            }
            else{
                int tk=(D[i]-1)/2;

                if(bsp.count(i)==0){
                    FOR(d,1,D[i]-1){
                        
                        scf[varCount]+=C[i]*(D[i]-d-1);
                        qcf[V[i][d-1]][varCount]+=C[i];
                        FOR(j,d,D[i]){
                            qcf[V[i][j]][varCount]+=-C[i];
                        }
                        ++varCount;
                    }
                    qcf[V[i][D[i]-2]][V[i][D[i]-1]]+=C[i];
                }
                else{
                    continue;
                    int mn=100000,nid=0,nid2=1;
                    rep(j1,D[i])FOR(j2,j1+1,D[i]){
                        if(qcf[V[i][j1]][V[i][j2]]!=0){
                            if(mn>abs(qcf[V[i][j1]][V[i][j2]]+C[i])){
                                mn=abs(qcf[V[i][j1]][V[i][j2]]+C[i]);
                                nid=j1,nid2=j2;
                            }
                        }
                    }
                    qcf[V[i][nid]][V[i][nid2]]+=C[i];
                }

            }
        }
        else if(D[i]==2){
            qcf[V[i][0]][V[i][1]]+=C[i];
        }
        else if(D[i]==1){
            scf[V[i][0]]+=C[i];
        }
        else if(D[i]==0){
            constant+=C[i];
            
        }
    }
    rep(i,K)if(!reducted[i]&&bsp.count(i)!=0){
        int mn=100000,nid=0,nid2=1;
        rep(j1,D[i])FOR(j2,j1+1,D[i]){
            if(qcf[V[i][j1]][V[i][j2]]!=0){
                if(mn>abs(qcf[V[i][j1]][V[i][j2]]+C[i])){
                    mn=abs(qcf[V[i][j1]][V[i][j2]]+C[i]);
                    nid=j1,nid2=j2;
                }
            }
        }
        qcf[V[i][nid]][V[i][nid2]]+=C[i];
    }
    rep(i,varCount){
        FOR(j,i+1,varCount)if(qcf[i][j]!=0){
            used[i]=true;
            used[j]=true;
        }
        if(scf[i]!=0) used[i]=true;
    }
    int offset=0;
    FOR(i,0,varCount){
        if(i<N) actvar[i]=i;
        else{
            if(used[i]){
                actvar[i]=i-offset;
            }
            else{
                ++offset;
            }
        }
    }
    int tmxcf=0;
    rep(i,varCount)FOR(j,i+1,varCount)if(qcf[i][j]!=0){
        AD.pb(2);
        AC.pb(qcf[i][j]);
        A1.pb(actvar[i]);A2.pb(actvar[j]);
        tmxcf=max(tmxcf,abs(qcf[i][j]));
    }
    rep(i,varCount)if(scf[i]!=0){
        AD.pb(1);
        AC.pb(scf[i]);
        A1.pb(actvar[i]);A2.pb(-1);
        tmxcf=max(tmxcf,abs(scf[i]));
    }
    if(constant!=0){
        AD.pb(0);
        AC.pb(constant);
        A1.pb(-1);
        A2.pb(-1);
    }
    int sz=AD.size();
    bestscore=pdscore(0,varCount-offset-N,sz,tmxcf);
    bestnewvar=varCount-offset;
    bA1=A1,bA2=A2,bAD=AD,bAC=AC;
    cerr<<bestscore<<endl;

    hdic.clear();
    memset(reducted,0,sizeof(reducted));
    memset(qcf,0,sizeof(qcf));
    memset(scf,0,sizeof(scf));
    constant=0;
    A1.clear();A2.clear();AD.clear();AC.clear();
    rep(i,K){
        msk[i]=0;
        V[i].clear();
    }
    K=initK;
    rep(i,K){
        int id=sorted[i].second;
        D[i]=tD[id],C[i]=tC[id];
        V[i]=tV[id];H[i]=tH[id];
        msk[i]=tmsk[id];
        hdic[H[i]]=i;
    }
    tmxcf=0;
    limit=300;
    mxcf=min(limit,mxcf);
    varCount=N;

    curscore=foolish();


    //cerr<<curscore<<endl;
    //if(bestscore<curscore){
        rep(i,K){
            if(D[i]==0||C[i]>0) constant+=C[i];
            else if(C[i]<0&&__builtin_popcountll(msk[i]&beststate)>0&&D[i]>2){
                int id1,id2;
                rep(lp,60){
                    while(id1=rnd.nextInt(D[i]),id2=rnd.nextInt(D[i]),id1==id2);
                    if(id1>id2) swap(id1,id2);
                    if(lp<10&&qcf[V[i][id1]][V[i][id2]]+C[i]>-150&&qcf[V[i][id1]][V[i][id2]]!=0) break;
                    else if(lp<25&&qcf[V[i][id1]][V[i][id2]]+C[i]>-150) break;
                    else if(lp<35&&qcf[V[i][id1]][V[i][id2]]+C[i]>-200&&qcf[V[i][id1]][V[i][id2]]!=0) break;
                    else if(lp<45&&qcf[V[i][id1]][V[i][id2]]+C[i]>-200) break;
                    else if(lp<55&&qcf[V[i][id1]][V[i][id2]]+C[i]>-300&&qcf[V[i][id1]][V[i][id2]]!=0) break;
                }
                qcf[V[i][id1]][V[i][id2]]+=C[i];
            }
            else if(C[i]<0&&__builtin_popcountll(msk[i]&beststate)==0&&D[i]>2){
                scf[varCount]+=-C[i]*(D[i]-1);
                rep(j,V[i].size()){
                    qcf[V[i][j]][varCount]+=C[i];
                }
                ++varCount;
            }
            else if(D[i]==2){
                qcf[V[i][0]][V[i][1]]+=C[i];
            }
            else if(D[i]==1){
                scf[V[i][0]]+=C[i];
            }
        }

    memset(used,0,sizeof(used));
    rep(i,varCount){
        FOR(j,i+1,varCount)if(qcf[i][j]!=0){
            used[i]=true;
            used[j]=true;
        }
        if(scf[i]!=0) used[i]=true;
    }
    offset=0;
    FOR(i,0,varCount){
        if(i<N) actvar[i]=i;
        else{
            if(used[i]){
                actvar[i]=i-offset;
            }
            else{
                ++offset;
            }
        }
    }
    rep(i,varCount)FOR(j,i+1,varCount)if(qcf[i][j]!=0){
        AD.pb(2);
        AC.pb(qcf[i][j]);
        A1.pb(actvar[i]);A2.pb(actvar[j]);
        tmxcf=max(tmxcf,abs(qcf[i][j]));
    }
    rep(i,varCount)if(scf[i]!=0){
        AD.pb(1);
        AC.pb(scf[i]);
        A1.pb(actvar[i]);A2.pb(-1);
        tmxcf=max(tmxcf,abs(scf[i]));
    }
    if(constant!=0){
        AD.pb(0);
        AC.pb(constant);
        A1.pb(-1);
        A2.pb(-1);
    }
    sz=AD.size();
    curscore=pdscore(0,varCount-offset-N,sz,tmxcf)/2;
    cerr<<curscore<<endl;
    if(bestscore<curscore){
        bestnewvar=varCount-offset;
        bA1=A1,bA2=A2,bAD=AD,bAC=AC;
    }
    

    cout<<bestnewvar<<" "<<bAD.size()<<endl;
    rep(i,bAD.size()){
        if(bAD[i]>=2)cout<<bAD[i]<<" "<<bAC[i]<<" "<<bA1[i]+1<<" "<<bA2[i]+1<<endl;
        else if(bAD[i]==1)cout<<bAD[i]<<" "<<bAC[i]<<" "<<bA1[i]+1<<endl;
        else cout<<bAD[i]<<" "<<bAC[i]<<endl;
    }
    return 0;
}
