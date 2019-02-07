#include <bits/stdc++.h>
#pragma GCC optimize ("-O3")
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

typedef pair<int,pint> pip;
const int dx[]={-1,0,1,0},dy[]={0,-1,0,1};
const int INF=1000100010;

double timeLimit=19.50;
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

struct Rand {
	uint32_t x,y,z,w;
	static constexpr double iDouble=1.0/(1LL<<32);
	Rand() {
		x=123456789;y=362436069;z=521288629;w=88675123;
	}
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

struct SPoint{
    int r,c,co;
    SPoint(){}
    SPoint(int r,int c,int co):r(r),c(c),co(co){}
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

//vset st(40001);
vset cdseed(1001);
const int DD=2048,SZ=200;
int avepx[3][270][270],gcnt[270][270];
int PXL[270][270][3];
vector<int> cpx[270][270][3];
class StainedGlass {
public:
    Timer timer;
    int H,W,N,pH,pW;
    int PR=3,BL=PR*PR;
    int LR,RR,LC,RC;
    int DIV=8;
    
    vector<SPoint> ans,bestans;
    set<pint> st;
    set<int> coset;
    
    int dvr[28],dvc[28];
    vector<pint> sdpo;
    bool visit[270][270];
    short seedco[1001],preseedco[1001];
    
    vector<short> bseed,prebseed;
    double bestscore;

    int cohist[128],avecol[128][3],cocnt[128],nxavecol[128][3];
    vector<int> colist;
    pint sort_colist[128];
    int seedarea[1001];
    int seedave[1001][3];
    int cosz,mxco;
    int mxdist[1001],nxmxdist[1001],sdscr[1001],nxsdscr[1001];
    int nxseedarea[1001],nxseedave[1001][3];
    int pretot=0,curtot=0;
    vector<int> cgsd;
    int mvd=8,mvd2=4;
    double getScore(){
        int tot=0,dif=0;
        rep(i,pH)rep(j,pW){
            int tid=seedco[bseed[i*pW+j]];
            dif=0;
            dif+=abs(avecol[tid][0]-PXL[i][j][0]);
            dif+=abs(avecol[tid][1]-PXL[i][j][1]);
            dif+=abs(avecol[tid][2]-PXL[i][j][2]);
            tot+=dif;
            sdscr[bseed[i*pW+j]]+=dif;
        }
        curtot=tot;
        double nco=coset.size();
        return (nco/N+1)*(nco/N+1)*tot;
    }
    double getScoredif(){
        int dtot=0,dif=0;
        queue<pip> que;
        memset(visit,0,sizeof(visit));
        for(int i:cgsd){
            que.push({i,sdpo[i]});
            visit[sdpo[i].first][sdpo[i].second]=true;
            nxsdscr[i]=0;
        }
        while(!que.empty()){
            pip pi=que.front();que.pop();
            int tid=pi.first,cr=pi.second.first,cc=pi.second.second;
            int tid2=seedco[tid];
            dif=0;
            dif+=abs(avecol[tid2][0]-PXL[cr][cc][0]);
            dif+=abs(avecol[tid2][1]-PXL[cr][cc][1]);
            dif+=abs(avecol[tid2][2]-PXL[cr][cc][2]);
            nxsdscr[tid]+=dif;

            rep(k,4){
                int tr=cr+dy[k],tc=cc+dx[k];
                if(tr>=0&&tr<pH&&tc>=0&&tc<pW&&!visit[tr][tc]){
                    if(cdseed.pos[bseed[tr*pW+tc]]!=-1){
                        visit[tr][tc]=true;
                        que.push({tid,{tr,tc}});
                    }
                }
            }
        }
        for(int i:cgsd){
            dtot+=nxsdscr[i]-sdscr[i];
        }
        curtot+=dtot;
        double nco=coset.size();
        return (nco/N+1)*(nco/N+1)*curtot;
    }
    inline bool forceupdate(double diff,double temp){
        if(diff>=0) return true;
        double d=(diff)*temp;
        if(d<-6) return false;
        return exp(d)>rnd.nextDouble();
    }
    void improve(){
        //double start=timer.get();
        double curscore=bestscore;
    
        //double starttemp=1750,endtemp=300,curtemp;
    
        int turn=0;
        double t;//invtl=1.0/timeLimit;
        pip memo;
        int cursz=cosz;
        memcpy(preseedco,seedco,sizeof(seedco));
        pretot=curtot;
        prebseed=bseed;
        while(1){
            ++turn;
            t=timer.get();
            if(t>timeLimit){
                cerr<<turn<<endl;
                return;
            }
            //curtemp=1.0/(starttemp+(endtemp-starttemp)*(t-start)*invtl);
            double nxscore=0;
            cgsd.clear();
            int sel=rnd.nextInt()&63;
            if(sel<=4){
            //if(sel<=-1){
                cdseed.clear();
                coset.clear();
                ans.clear();
                if(sel==0||(sel<2&&cursz==mxco)){
                    int pp=(rnd.nextInt()&1)+1;
                    if(cursz-pp<1) continue;
                    cursz-=pp;
                    rep(i,N){
                        int tco=0;
                        int cr=seedave[i][2],cg=seedave[i][1],cb=seedave[i][0];
                        int diff,ttid=-1;
                        int mn=INF;
                        rep(k,cursz){
                            diff=abs(cr-avecol[k][2])+abs(cg-avecol[k][1])+abs(cb-avecol[k][0]);
                            if(mn>diff) mn=diff,ttid=k;
                        }
                        if(seedco[i]>=cursz){
                            cgsd.pb(i);
                            cdseed.insert(i);
                        }
                        tco=avecol[ttid][0];
                        tco|=avecol[ttid][1]<<8;
                        tco|=avecol[ttid][2]<<16;
                        seedco[i]=ttid;
                        coset.insert(tco);
                        ans.pb(SPoint(sdpo[i].first*PR+2,sdpo[i].second*PR+2,tco));
                        
                    }
            
                    nxscore=getScoredif();

                    if(nxscore<=curscore){
                        curscore=nxscore;
                        memcpy(preseedco,seedco,sizeof(seedco));
                        for(int ii:cgsd)sdscr[ii]=nxsdscr[ii];
                        pretot=curtot;
                        if(nxscore<bestscore){
                            bestans=ans;
                            bestscore=nxscore;
                            //cerr<<1<<timer.get()<<" "<<bestscore<<endl;
                        }
                    }
                    else{
                        cursz+=pp;
                        curtot=pretot;
                        memcpy(seedco,preseedco,sizeof(preseedco));
                    }
                }
                else if(sel==1&&cursz<mxco){
                    int pp=rnd.nextInt(3)+1;
                    pp=min(mxco-cursz,pp);
                    cursz+=pp;
                    rep(i,N){
                        int tco=0;
                        int cr=seedave[i][2],cg=seedave[i][1],cb=seedave[i][0];
                        int diff,ttid=-1;
                        int mn=INF;
                        
                        rep(k,cursz){
                            diff=abs(cr-avecol[k][2])+abs(cg-avecol[k][1])+abs(cb-avecol[k][0]);
                            if(mn>diff) mn=diff,ttid=k;
                        }
                        if(cursz-pp<=ttid){
                            cgsd.pb(i);
                            cdseed.insert(i);
                        }
                        tco=avecol[ttid][0];
                        tco|=avecol[ttid][1]<<8;
                        tco|=avecol[ttid][2]<<16;
                        seedco[i]=ttid;
                        coset.insert(tco);
                        ans.pb(SPoint(sdpo[i].first*PR+2,sdpo[i].second*PR+2,tco));
                    }
            
                    nxscore=getScoredif();

                    if(nxscore<=curscore){
                        curscore=nxscore;
                        memcpy(preseedco,seedco,sizeof(seedco));
                        for(int ii:cgsd)sdscr[ii]=nxsdscr[ii];
                        pretot=curtot;
                        if(nxscore<bestscore){
                            bestans=ans;
                            bestscore=nxscore;
                            //cerr<<2<<timer.get()<<" "<<bestscore<<endl;
                        }
                    }
                    else{
                        curtot=pretot;
                        cursz-=pp;
                        memcpy(seedco,preseedco,sizeof(preseedco));
                    }
                }
                else{
                    int tid=rnd.nextInt(cursz);
                    int nx=rnd.nextInt(3);
                    int cgw=(rnd.nextInt(8))-4;
                    cgw=max(avecol[tid][nx]-255,min(avecol[tid][nx],cgw));
                    if(cgw==0) continue;
                    avecol[tid][nx]-=cgw;
                    rep(i,N){
                        int tco=0;
                        int cr=seedave[i][2],cg=seedave[i][1],cb=seedave[i][0];
                        int diff,ttid=-1;
                        int mn=INF;
                        rep(k,cursz){
                            diff=abs(cr-avecol[k][2])+abs(cg-avecol[k][1])+abs(cb-avecol[k][0]);
                            if(mn>diff) mn=diff,ttid=k;
                        }
                        if(seedco[i]==tid||ttid==tid){
                            cgsd.pb(i);
                            cdseed.insert(i);
                        }
                        tco=avecol[ttid][0];
                        tco|=avecol[ttid][1]<<8;
                        tco|=avecol[ttid][2]<<16;
                        seedco[i]=ttid;
                        coset.insert(tco);
                        ans.pb(SPoint(sdpo[i].first*PR+2,sdpo[i].second*PR+2,tco));
                    }
            
                    nxscore=getScoredif();

                    if(nxscore<=curscore){
                        curscore=nxscore;
                        memcpy(preseedco,seedco,sizeof(seedco));
                        for(int ii:cgsd)sdscr[ii]=nxsdscr[ii];
                        pretot=curtot;
                        if(nxscore<bestscore){
                            bestans=ans;
                            bestscore=nxscore;
                            //cerr<<2<<timer.get()<<" "<<bestscore<<endl;
                        }
                    }
                    else{
                        curtot=pretot;
                        avecol[tid][nx]+=cgw;
                        memcpy(seedco,preseedco,sizeof(preseedco));
                    }
                }
                
                
            }
            else{
                
                int id=rnd.nextInt(N);
                

                memo={id,sdpo[id]};
                st.erase(sdpo[id]);
                while(1){
                    int pr=sdpo[id].first+rnd.nextInt(mvd)-mvd2,pc=sdpo[id].second+rnd.nextInt(mvd)-mvd2;
                    pr=max(LR,min(pr,RR));pc=max(LC,min(pc,RC));
                    if(st.count({pr,pc})==0){
                        sdpo[id]={pr,pc};
                        st.insert({pr,pc});
                        break;
                    }
                }

                queue<pip> que;
                memset(visit,0,sizeof(visit));
                cdseed.clear();
                int cr=sdpo[id].first,cc=sdpo[id].second;
                cgsd.clear();
                rep(i,N){
                    int tr=sdpo[i].first,tc=sdpo[i].second,dlim=max(mxdist[id],mxdist[i])*2+5;
                    if(dlim>abs(tr-cr)+abs(tc-cc)){
                        que.push({i,sdpo[i]});
                        visit[sdpo[i].first][sdpo[i].second]=true;
                        cgsd.pb(i);
                        nxmxdist[i]=0;
                        nxseedave[i][0]=0;nxseedave[i][1]=0;nxseedave[i][2]=0;
                        nxseedarea[i]=0;
                        cdseed.insert(i);
                    }
                    
                }
                //memset(seedave,0,sizeof(seedave));
                //memset(seedarea,0,sizeof(seedarea));
                
                while(!que.empty()){
                    pip pi=que.front();que.pop();
                    int tid=pi.first,cr=pi.second.first,cc=pi.second.second;
                    nxmxdist[tid]=max(nxmxdist[tid],abs(cr-sdpo[tid].first)+abs(cc-sdpo[tid].second));
                    ++nxseedarea[tid];
                    nxseedave[tid][0]+=(PXL[cr][cc][0]);
                    nxseedave[tid][1]+=(PXL[cr][cc][1]);
                    nxseedave[tid][2]+=(PXL[cr][cc][2]);
                    bseed[cr*pW+cc]=tid;
                    rep(k,4){
                        int tr=cr+dy[k],tc=cc+dx[k];
                        if(tr>=0&&tr<pH&&tc>=0&&tc<pW&&!visit[tr][tc]){
                            if(cdseed.pos[bseed[tr*pW+tc]]!=-1){
                                visit[tr][tc]=true;
                                que.push({tid,{tr,tc}});
                            }
                        }
                    }
                }
                coset.clear();
                ans.clear();
                rep(i,N){
                    int tco=0;
                    if(cdseed.pos[i]==-1){
                        int tid=seedco[i];
                        tco=avecol[tid][0];
                        tco|=avecol[tid][1]<<8;
                        tco|=avecol[tid][2]<<16;
                    }
                    else{

                        nxseedave[i][0]/=nxseedarea[i];
                        nxseedave[i][1]/=nxseedarea[i];
                        nxseedave[i][2]/=nxseedarea[i];
                        int cr=nxseedave[i][2],cg=nxseedave[i][1],cb=nxseedave[i][0];
                        int diff,tid=-1;
                        int mn=INF;
                        rep(k,cursz){
                            diff=abs(cr-avecol[k][2])+abs(cg-avecol[k][1])+abs(cb-avecol[k][0]);
                            if(mn>diff) mn=diff,tid=k;
                        }
                        tco=avecol[tid][0];
                        tco|=avecol[tid][1]<<8;
                        tco|=avecol[tid][2]<<16;
                        seedco[i]=tid;
                        
                    }
                    coset.insert(tco);
                    ans.pb(SPoint(sdpo[i].first*PR+2,sdpo[i].second*PR+2,tco));
                }
            
                nxscore=getScoredif();

                if(nxscore<=curscore){
                    curscore=nxscore;
                    memcpy(preseedco,seedco,sizeof(seedco));
                    prebseed=bseed;
                    for(int ii:cgsd){
                        mxdist[ii]=nxmxdist[ii];
                        seedave[ii][0]=nxseedave[ii][0];
                        seedave[ii][1]=nxseedave[ii][1];
                        seedave[ii][2]=nxseedave[ii][2];
                        sdscr[ii]=nxsdscr[ii];
                        pretot=curtot;
                    }
                    if(nxscore<bestscore){
                        bestans=ans;
                        bestscore=nxscore;
                        //cerr<<timer.get()<<" "<<bestscore<<endl;
                    }
                }
                else{
                        int id=memo.first;
                        st.erase(sdpo[id]);
                        pint nx=memo.second;
                        st.insert(nx);
                        sdpo[id]=nx;
                        memcpy(seedco,preseedco,sizeof(preseedco));
                        curtot=pretot;
                        bseed=prebseed;
                }
            }
        }
    }
    double stdev[28][28];
    int numpo[28][28];
    
    vector<int> create(int h, vector<int> pixels, int n) {
        timer.reset();
        H=h,W=pixels.size()/H,N=n;
        if(H*W/N>=1600) mvd=12,mvd2=6;
        pH=(H+PR-1)/PR;pW=(W+PR-1)/PR;
        LR=0,RR=pH-1,LC=0,RC=pW-1;
        //LR=-1000/PR,RR=pH+999/PR,LC=-1000/PR,RC=pW+999/PR;
        ans.reserve(N);
        bseed.resize(pH*pW);

       rep(i,H*W){
            int tr=i/W/PR,tc=i%W/PR;
            avepx[0][tr][tc]+=(pixels[i]&0xff);
            avepx[1][tr][tc]+=((pixels[i]>>8)&0xff);
            avepx[2][tr][tc]+=((pixels[i]>>16)&0xff);
            cpx[tr][tc][0].pb(pixels[i]&0xff);
            cpx[tr][tc][1].pb((pixels[i]>>8)&0xff);
            cpx[tr][tc][2].pb((pixels[i]>>16)&0xff);
            ++gcnt[tr][tc];
        }
        
        rep(i,pH)rep(j,pW){
            //int tid=bseed[i*pW+j];
            int tbl=gcnt[i][j],hist=0;
            //PXL[i][j][0]=(avepx[0][i][j]/tbl);
            //PXL[i][j][1]=(avepx[1][i][j]/tbl);
            //PXL[i][j][2]=(avepx[2][i][j]/tbl);
            sort(cpx[i][j][0].begin(),cpx[i][j][0].end());
            PXL[i][j][0]=cpx[i][j][0][tbl/2];
            sort(cpx[i][j][1].begin(),cpx[i][j][1].end());
            PXL[i][j][1]=cpx[i][j][1][tbl/2];
            sort(cpx[i][j][2].begin(),cpx[i][j][2].end());
            PXL[i][j][2]=cpx[i][j][2][tbl/2];
            hist=(avepx[0][i][j]/tbl)/64;
            hist+=(avepx[1][i][j]/tbl)/64*4;
            hist+=(avepx[2][i][j]/tbl)/64*16;
            ++cohist[hist];
        }

        int rr=pH%(pH/10),cr=pW%(pW/10);
        FOR(i,1,pH/10+1){
            dvr[i]=dvr[i-1]+10+(rr>=i?1:0);
        }
        FOR(i,1,pW/10+1){
            dvc[i]=dvc[i-1]+10+(cr>=i?1:0);
        }

        double totdev=0;
        rep(i,pH/10)rep(j,pW/10){
            double s1=0,s2=0,s3=0;
            int num=(dvr[i+1]-dvr[i])*(dvc[j+1]-dvc[j]);
            FOR(k1,dvr[i],dvr[i+1])FOR(k2,dvc[j],dvc[j+1]){
                s1+=PXL[k1][k2][0];s2+=PXL[k1][k2][1];s3+=PXL[k1][k2][2];
            }
            s1/=num;s2/=num;s3/=num;
            double tdev=0;
            FOR(k1,dvr[i],dvr[i+1])FOR(k2,dvc[j],dvc[j+1]){
                int sq=s1-PXL[k1][k2][0];
                tdev+=sq*sq;
                sq=s2-PXL[k1][k2][1];
                tdev+=sq*sq;
                sq=s3-PXL[k1][k2][2];
                tdev+=sq*sq;
            }
            
            stdev[i][j]=sqrt(tdev/(num*3));
            //cerr<<stdev[i][j]<<endl;
            totdev+=stdev[i][j];
        }
        int curseed=0;
        rep(i,pH/10)rep(j,pW/10){
            numpo[i][j]=min(90,(int)(stdev[i][j]/totdev*N));
            curseed+=numpo[i][j];
            int pr,pc;
            //cerr<<numpo[i][j]<<endl;
            while(numpo[i][j]!=0){
                while(1){
                    pr=rnd.nextInt(dvr[i+1]-dvr[i])+dvr[i],pc=rnd.nextInt(dvc[j+1]-dvc[j])+dvc[j];
                    if(st.count({pr,pc})==0) break;
                }
                st.insert({pr,pc});
                sdpo.pb(pr,pc);
                --numpo[i][j];
            }
        }
        //cerr<<curseed<<endl;
        FOR(i,curseed,N){
            int p;
            while(p=rnd.nextInt(H*W),st.count({p/W/PR,p%W/PR})!=0);
            st.insert({p/W/PR,p%W/PR});
            sdpo.pb(p/W/PR,p%W/PR);
        }
        
        rep(i,64){
            sort_colist[i]={cohist[i],i};
        }
        sort(sort_colist,sort_colist+64,greater<pint>());
        
        
        rep(i,32){
            if(sort_colist[i].first>=2){
                int ii=sort_colist[i].second;
                colist.pb(ii);
                int id=colist.size()-1;
                avecol[id][0]=ii%4*64+32;
                avecol[id][1]=ii/4%4*64+32;
                avecol[id][2]=ii/16*64+32;
            }
        }
        /*
        rep(i,512){
            if(cohist[i]>=2){
                colist.pb(i);
                avecol[i][0]=i%8*32+16;
                avecol[i][1]=i/8%8*32+16;
                avecol[i][2]=i/64*32+16;
            }
        }
        */
        cosz=colist.size();
        mxco=cosz;
        //cerr<<cosz<<endl;
        bool update=true;
        rep(loop,100){
            if(!update) break;
            memset(nxavecol,0,sizeof(nxavecol));
            memset(cocnt,0,sizeof(cocnt));
            update=false;
            int cr,cg,cb,diff,tid=-1;
            rep(i,pH)rep(j,pW){
                int mn=INF;
                cr=PXL[i][j][2],cg=PXL[i][j][1],cb=PXL[i][j][0];
                rep(k,cosz){
                    diff=abs(cr-avecol[k][2])+abs(cg-avecol[k][1])+abs(cb-avecol[k][0]);
                    if(mn>diff) mn=diff,tid=k;
                }
                nxavecol[tid][2]+=cr;nxavecol[tid][1]+=cg;nxavecol[tid][0]+=cb;
                ++cocnt[tid];
            }
            rep(i,cosz)if(cocnt[i]!=0){
                nxavecol[i][0]/=cocnt[i];nxavecol[i][1]/=cocnt[i];nxavecol[i][2]/=cocnt[i];
                if(avecol[i][0]!=nxavecol[i][0]||avecol[i][1]!=nxavecol[i][1]||avecol[i][2]!=nxavecol[i][2]){
                    update=true;
                    avecol[i][0]=nxavecol[i][0];
                    avecol[i][1]=nxavecol[i][1];
                    avecol[i][2]=nxavecol[i][2];
                }
            }
        }
        
        //cosz=min(16,(int)colist.size());

        //!
        cosz=16;

        queue<pip> que;
        memset(visit,0,sizeof(visit));
        rep(i,N){
            que.push({i,sdpo[i]});
            visit[sdpo[i].first][sdpo[i].second]=true;
        }
        while(!que.empty()){
            pip pi=que.front();que.pop();
            int tid=pi.first,cr=pi.second.first,cc=pi.second.second;
            mxdist[tid]=max(mxdist[tid],abs(cr-sdpo[tid].first)+abs(cc-sdpo[tid].second));
            ++seedarea[tid];
            seedave[tid][0]+=PXL[cr][cc][0];
            seedave[tid][1]+=PXL[cr][cc][1];
            seedave[tid][2]+=PXL[cr][cc][2];
            bseed[cr*pW+cc]=tid;
            rep(k,4){
                int tr=cr+dy[k],tc=cc+dx[k];
                if(tr>=0&&tr<pH&&tc>=0&&tc<pW&&!visit[tr][tc]){
                    visit[tr][tc]=true;
                    que.push({tid,{tr,tc}});
                }
            }
        }

        rep(i,N){
            int tco=0;
            seedave[i][0]/=seedarea[i];
            seedave[i][1]/=seedarea[i];
            seedave[i][2]/=seedarea[i];
            int cr=seedave[i][2],cg=seedave[i][1],cb=seedave[i][0];
            int diff,tid=-1;
            int mn=INF;
            rep(k,cosz){
                diff=abs(cr-avecol[k][2])+abs(cg-avecol[k][1])+abs(cb-avecol[k][0]);
                if(mn>diff) mn=diff,tid=k;
            }
            tco=avecol[tid][0];
            tco|=avecol[tid][1]<<8;
            tco|=avecol[tid][2]<<16;
            seedco[i]=tid;
            coset.insert(tco);
            ans.pb(SPoint(sdpo[i].first*PR+2,sdpo[i].second*PR+2,tco));
        }
        
        double cscr=getScore();
        bestscore=cscr;
        bestans=ans;

        //cerr<<bestscore<<endl;
        improve();
        cerr<<bestscore<<endl;
        cerr<<coset.size()<<endl;
        vector<int> ret;
        for(auto it:bestans){
            ret.pb(it.r);ret.pb(it.c);ret.pb(it.co);
        }
        return ret;
    }
};
// -------8<------- end of the solution submitted to the website -------8<-------

template<class T> void getVector(vector<T>& v) {
    for (int i = 0; i < (int)v.size(); ++i)
        cin >> v[i];
}

int main() {
    StainedGlass sg;
    int H;
    cin >> H;
    int S;
    cin >> S;
    vector<int> pixels(S);
    getVector(pixels);
    
    int N;
    cin >> N;
    
    vector<int> ret = sg.create(H, pixels, N);
    cout << ret.size() << endl;
    for (int i = 0; i < (int)ret.size(); ++i)
        cout << ret[i] << endl;
    cout.flush();
}