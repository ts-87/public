#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

#define eps (1e-10)

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

double timeLimit=9.9;
//double timeLimit2=1.0;
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
Timer timer;
int H,W,N,rosecount=0;
int SQA,SQR;
bool isrose[80][80];
int num_vline,num_hline;
vector<int> vl,bestvl;
vector<pint> hly[80],besthly[80];
int rose_vsum[81][81];
double bestscore=1000100010;
vector<double> areadata[80],rosedata[80];
vector<double> bestarea[80],bestrose[80];
double bestsqarea,bestsqrose;
class CakeSharing {
public:
    void update_best(double cur,bool flag=false){
        bestscore=cur;
        bestvl=vl;
        rep(i,vl.size()-1){
            besthly[i]=hly[i];
            if(flag) bestarea[i]=areadata[i],bestrose[i]=rosedata[i];
        }
        //cerr<<timer.get()<<" "<<bestscore<<endl;
    }
    inline bool forceupdate(double sub,double temp){
        if(sub>=0) return true;
        double d=sub*temp;
        //double d=-sqrt(-sub)*temp;
        if(d<-6) return false;
        return exp(d)>(double)rnd.nextDouble();
    }
    vector<double> tmparea1,tmparea2,tmprose1,tmprose2;
    void improve1(int sqarea,int sqrose){
        double limit=timer.get()+0.1;
        double curscore=bestscore,nxscore;
        double cur_sqarea=sqarea,cur_sqrose=sqrose;
        bestsqarea=cur_sqarea;
        bestsqrose=cur_sqrose;
        double starttemp=10,endtemp=1,curtemp;
        int turn=-1;
        int hsz=vl.size()-1;
        double invtl=1.0/limit;
        while(1){
            ++turn;
            double t=timer.get();
            if(t>limit){
                //cerr<<turn<<endl;
                return;
            }
            curtemp=1.0/(starttemp+(endtemp-starttemp)*t*invtl);
            int tx,ty,mv,choice;
            while(1){
                choice=rnd.nextInt(10);
                if(choice==0&&hsz>1){
                    tx=rnd.nextInt(hsz-1)+1;
                    mv=(rnd.nextInt()&1)*2-1;
                    if(vl[tx]+mv<vl[tx+1]&&vl[tx]+mv>vl[tx-1]) break;
                }
                else{
                    if(choice==0) choice=1;
                    tx=rnd.nextInt(hsz);
                    if(hly[tx].size()-2<=0) continue;
                    ty=rnd.nextInt(hly[tx].size()-2)+1;
                    mv=(rnd.nextInt()&1)*2-1;
                    if(hly[tx][ty].first+mv<hly[tx][ty+1].first
                    &&hly[tx][ty].first+mv>hly[tx][ty-1].first) break;
                }
            }
            double prearea=0,prerose=0,nxarea=0,nxrose=0;
            if(choice==0){
                rep(i,areadata[tx-1].size()){
                    prearea+=areadata[tx-1][i]*areadata[tx-1][i];
                    prerose+=rosedata[tx-1][i]*rosedata[tx-1][i];
                }
                rep(i,areadata[tx].size()){
                    prearea+=areadata[tx][i]*areadata[tx][i];
                    prerose+=rosedata[tx][i]*rosedata[tx][i];
                }
                tmparea1.clear(),tmparea2.clear(),tmprose1.clear(),tmprose2.clear();
                vl[tx]+=mv;
                rep(i,hly[tx-1].size()-1){
                    tmparea1.pb((hly[tx-1][i+1].first-hly[tx-1][i].first)*(vl[tx]-vl[tx-1]));
                    nxarea+=tmparea1.back()*tmparea1.back();
                    int sum=0;
                    int h0=hly[tx-1][i].first,h1=hly[tx-1][i+1].first;
                    FOR(k,vl[tx-1],vl[tx]){
                        sum+=rose_vsum[k][h1]-rose_vsum[k][h0];
                    }
                    tmprose1.pb(sum);
                    nxrose+=sum*sum;
                }
                rep(i,hly[tx].size()-1){
                    tmparea2.pb((hly[tx][i+1].first-hly[tx][i].first)*(vl[tx+1]-vl[tx]));
                    nxarea+=tmparea2.back()*tmparea2.back();
                    int sum=0;
                    int h0=hly[tx][i].first,h1=hly[tx][i+1].first;
                    FOR(k,vl[tx],vl[tx+1]){
                        sum+=rose_vsum[k][h1]-rose_vsum[k][h0];
                    }
                    tmprose2.pb(sum);
                    nxrose+=sum*sum;
                }
                nxarea=cur_sqarea-prearea+nxarea;
                nxrose=cur_sqrose-prerose+nxrose;

                nxscore=(1.0+sqrt(nxarea*N-SQA)/N)*(1.0+sqrt(nxrose*N-SQR)/N);

                if(forceupdate(curscore-nxscore,curtemp)){
                    curscore=nxscore,cur_sqarea=nxarea,cur_sqrose=nxrose;
                    areadata[tx-1]=tmparea1,areadata[tx]=tmparea2;
                    rosedata[tx-1]=tmprose1,rosedata[tx]=tmprose2;
                    if(bestscore>curscore){
                        update_best(curscore,true);
                        bestsqarea=cur_sqarea;
                        bestsqrose=cur_sqrose;
                    } 
                }
                else{
                    vl[tx]-=mv;
                }

            }
            else{
                double tmp1=areadata[tx][ty-1],tmp2=areadata[tx][ty];
                prearea+=tmp1*tmp1;
                prearea+=tmp2*tmp2;
                prerose=rosedata[tx][ty-1]*rosedata[tx][ty-1]+rosedata[tx][ty]*rosedata[tx][ty];

                hly[tx][ty].first+=mv;
                hly[tx][ty].second+=mv;

                tmp1=(hly[tx][ty].first-hly[tx][ty-1].first)*(vl[tx+1]-vl[tx]);
                nxarea+=tmp1*tmp1;
                tmp2=(hly[tx][ty+1].first-hly[tx][ty].first)*(vl[tx+1]-vl[tx]);
                nxarea+=tmp2*tmp2;

                int nxrose1=0,nxrose2=0;
                int h0=hly[tx][ty-1].first,h1=hly[tx][ty].first,h2=hly[tx][ty+1].first;
                FOR(k,vl[tx],vl[tx+1]){
                    nxrose1+=rose_vsum[k][h1]-rose_vsum[k][h0];
                    nxrose2+=rose_vsum[k][h2]-rose_vsum[k][h1];
                }
                nxrose=nxrose1*nxrose1+nxrose2*nxrose2;
                nxarea=cur_sqarea-prearea+nxarea;
                nxrose=cur_sqrose-prerose+nxrose;

                nxscore=(1.0+sqrt(nxarea*N-SQA)/N)*(1.0+sqrt(nxrose*N-SQR)/N);
                if(forceupdate(curscore-nxscore,curtemp)){
                    curscore=nxscore,cur_sqarea=nxarea,cur_sqrose=nxrose;
                    areadata[tx][ty-1]=tmp1,areadata[tx][ty]=tmp2;
                    rosedata[tx][ty-1]=nxrose1,rosedata[tx][ty]=nxrose2;
                    if(bestscore>curscore){
                        update_best(curscore,true);
                        bestsqarea=cur_sqarea;
                        bestsqrose=cur_sqrose;
                    } 
                }
                else{
                    hly[tx][ty].first-=mv;
                    hly[tx][ty].second-=mv;
                }
            }

            
        }
    }
    vector<double> tmparea[80],tmprose[80];
    vector<pint> tmphly[80];
    void improve2(){
        double tmpscore=bestscore,nxscore;
        double tmp_sqarea=bestsqarea,tmp_sqrose=bestsqrose;
        rep(i,bestvl.size()-1){
            tmparea[i]=bestarea[i],tmprose[i]=bestrose[i];
            tmphly[i]=besthly[i];
        }
        vl=bestvl;
        double starttemp=0.1,endtemp=0.001,curtemp;
        int turn=-1;//,cur_rcnt=rosecount;
        int hsz=vl.size()-1;
        int loop=3;
        if(H<30&&N<100) loop=4;
        double invtl=(double)loop/timeLimit;
        
        rep(pp,loop){
            double curscore=tmpscore;
            double cur_sqarea=tmp_sqarea,cur_sqrose=tmp_sqrose;
            rep(i,bestvl.size()-1){
                areadata[i]=tmparea[i],rosedata[i]=tmprose[i];
                hly[i]=tmphly[i];
            }
            int cur_rcnt=rosecount;
            double t_limit=timeLimit/loop*(pp+1);
        while(1){
            ++turn;
            double t=timer.get();
            if(t>t_limit){
                //cerr<<turn<<endl;
                break;
            }
            curtemp=1.0/(starttemp+(endtemp-starttemp)*(t-timeLimit/loop*pp)*invtl);
            int tx,ty,lorr,mv;
            int ul,ur,ml,mr,dl,dr;
            while(1){
                tx=rnd.nextInt(hsz);
                if(hly[tx].size()-2<=0) continue;
                ty=rnd.nextInt(hly[tx].size()-2)+1;
                lorr=rnd.nextInt()%3;
                mv=(rnd.nextInt()&1)*2-1;
                ul=hly[tx][ty-1].first;
                ur=hly[tx][ty-1].second;
                ml=hly[tx][ty].first;
                mr=hly[tx][ty].second;
                dl=hly[tx][ty+1].first;
                dr=hly[tx][ty+1].second;
                if(lorr==2){
                    if(ml+mv<=dl
                    &&ml+mv>=ul
                    &&mr-mv<=dr
                    &&mr-mv>=ur) break;
                }
                else if(lorr==1){
                    if(ml+mv<=dl
                    &&ml+mv>=ul
                    &&(ml+mv!=dl||mr!=dr)
                    &&(ml+mv!=ul||mr!=ur)) break;
                }
                else{
                    if(mr+mv<=dr
                    &&mr+mv>=ur
                    &&(ml!=dl||mr+mv!=dr)
                    &&(ml!=ul||mr+mv!=ur)) break;
                }
            }

            double prearea=0,prerose=0;
            double tmp1=areadata[tx][ty-1],tmp2=areadata[tx][ty];
            int prerose1=rosedata[tx][ty-1],prerose2=rosedata[tx][ty];
            prearea+=tmp1*tmp1;
            prearea+=tmp2*tmp2;
            prerose=prerose1*prerose1+prerose2*prerose2;


            if(lorr==2){
                ml+=mv;
                mr-=mv;
            }
            else if(lorr==1) ml+=mv;
            else  mr+=mv;

            double nxarea=0,nxrose=0;
            int nxrose1=0,nxrose2=0;
            tmp1=0.5*(ml-ul+mr-ur)*(vl[tx+1]-vl[tx]);
            nxarea+=tmp1*tmp1;
            tmp2=0.5*(dl-ml+dr-mr)*(vl[tx+1]-vl[tx]);
            nxarea+=tmp2*tmp2;
            double dy1=(double)(mr-ml)/(vl[tx+1]-vl[tx]);
            double dy0=(double)(ur-ul)/(vl[tx+1]-vl[tx]);
            double dy2=(double)(dr-dl)/(vl[tx+1]-vl[tx]);
            int h1,h2,h3,h4;
            FOR(k,vl[tx],vl[tx+1]){
                int k2=k-vl[tx];
                h1=(0.5+k2)*dy1+ml+0.5-eps;
                h2=(0.5+k2)*dy0+ul+0.5+eps;
                h3=(0.5+k2)*dy2+dl+0.5-eps;
                h4=(0.5+k2)*dy1+ml+0.5+eps;
                nxrose1+=rose_vsum[k][h1]-rose_vsum[k][h2];
                nxrose2+=rose_vsum[k][h3]-rose_vsum[k][h4];
            }
            nxrose=nxrose1*nxrose1+nxrose2*nxrose2;
            int nxrcnt=cur_rcnt-prerose1-prerose2+nxrose1+nxrose2;

            nxarea=cur_sqarea-prearea+nxarea;
            nxrose=cur_sqrose-prerose+nxrose;
            nxscore=(1.0+sqrt(nxarea*N-SQA)/N)*(1.0+sqrt(nxrose*N-nxrcnt*nxrcnt)/N);
            if(forceupdate(curscore-nxscore,curtemp)){
                curscore=nxscore,cur_sqarea=nxarea,cur_sqrose=nxrose,cur_rcnt=nxrcnt;
                areadata[tx][ty-1]=tmp1,areadata[tx][ty]=tmp2;
                rosedata[tx][ty-1]=nxrose1,rosedata[tx][ty]=nxrose2;
                if(lorr==2){
                    hly[tx][ty].first=ml;
                    hly[tx][ty].second=mr;
                }
                else if(lorr) hly[tx][ty].first=ml;
                else  hly[tx][ty].second=mr;
               if(bestscore>curscore){
                   update_best(curscore);
                } 
            }
            else{
                /*
                if(lorr==2){
                    hly[tx][ty].first-=mv;
                    hly[tx][ty].second+=mv;
                }
                else if(lorr) hly[tx][ty].first-=mv;
                else  hly[tx][ty].second-=mv;
                */
            }
        }
        }
    }
    vector<string> cut(vector<string> roses, int NP) {
        timer.reset();
        N=NP;
        H=roses.size(),W=roses[0].size();
        
        rep(j,W)rep(i,H){
            int c=0;
            if(roses[i][j]=='R'){
                isrose[i][j]=true;
                ++rosecount;
                c=1;
            }
            rose_vsum[j][i+1]+=c+rose_vsum[j][i];
        }
        SQA=H*H*W*W;
        SQR=rosecount*rosecount;
        FOR(ln,0,W){
        //num_vline=sqrt(NP)-1;
        //num_hline=NP-1-num_vline;
        num_vline=ln;
        num_hline=NP-1-num_vline;
        int md=num_hline%(num_vline+1),hcnt=num_hline/(num_vline+1);
        //if(hcnt<1) break;
        if(hcnt+1>H-1||num_hline<=0) continue;
        int sqarea=0,sqrose=0,C=num_vline+1;
        if((C-md)*hcnt+md*(hcnt+1)<=0) continue;
        double w1=(double)W*hcnt/((C-md)*hcnt+md*(hcnt+1));
        double w2=(double)W*(hcnt+1)/((C-md)*hcnt+md*(hcnt+1));

        vl.clear();

        vl.pb(0);
        rep(i,num_vline){
            if(i<C-md) vl.pb(round(w1*(i+1)));
            else vl.pb(round(w1*(C-md)+w2*(i+1-C+md)));
            //vl.pb(vl.back()+W/(num_vline+1)+((i>(num_vline+1)-md)?1:0));
        }
        vl.pb(W);

        rep(i,vl.size()-1) hly[i].clear(),areadata[i].clear(),rosedata[i].clear();

        rep(i,vl.size()-1){
            int h=hcnt+((i>=(int)vl.size()-1-md)?1:0);
            int carea,crose,height,preh;
            hly[i].pb(0,0);
            rep(j,h){
                carea=0,crose=0;
                height=round((double)H/(h+1)*(j+1));
                preh=hly[i].back().first;
                carea=(height-preh)*(vl[i+1]-vl[i]);

                areadata[i].pb(carea);

                carea*=carea;
                FOR(k,vl[i],vl[i+1]){
                    crose+=rose_vsum[k][height]-rose_vsum[k][preh];
                }

                rosedata[i].pb(crose);

                crose*=crose;
                hly[i].pb(height,height);
                sqarea+=carea,sqrose+=crose;
            }
            carea=0,crose=0;
            height=H;
            carea=(height-hly[i].back().first)*(vl[i+1]-vl[i]);

            areadata[i].pb(carea);

            carea*=carea;
            FOR(k,vl[i],vl[i+1]){
                crose+=rose_vsum[k][height]-rose_vsum[k][hly[i].back().first];
            }

            rosedata[i].pb(crose);

            crose*=crose;
            hly[i].pb(H,H);
            sqarea+=carea,sqrose+=crose;
        }
        double tscore=1.0+1.0/NP*sqrt((double)NP*sqarea-SQA);
        tscore*=(1.0+1.0/NP*sqrt((double)NP*sqrose-SQR));
        if(tscore<bestscore+0.5){
            if(tscore<bestscore){
                update_best(tscore,true);
                bestsqarea=sqarea,bestsqrose=sqrose;
            }
            if(NP<50)improve1(sqarea,sqrose);
        }
        }
        //cerr<<timer.get()<<endl;
        /*
        bestvl=vl;
        rep(i,vl.size()-1) besthly[i]=hly[i];
        */
        //improve1(sqarea,sqrose);
        improve2();

        cerr<<bestscore<<endl;

        vector<string> ans;
        FOR(i,1,bestvl.size()-1){
            string tmp;
            tmp+=to_string(bestvl[i])+" 0 "+to_string(bestvl[i])+" "+to_string(H);
            ans.pb(tmp);
        }
        rep(i,bestvl.size()-1)FOR(j,1,besthly[i].size()-1){
            string tmp;
            int nxt=bestvl[i+1];
            tmp+=to_string(bestvl[i])+" "+to_string(besthly[i][j].first)+" "+to_string(nxt)+" "+to_string(besthly[i][j].second);
            ans.pb(tmp);
        }
        /*
        rep(i,ans.size()){
            cerr<<i<<" "<<ans[i]<<endl;
        }
        */
        return ans;
    }
};
// -------8<------- end of solution submitted to the website -------8<-------

template<class T> void getVector(vector<T>& v) {
    for (int i = 0; i < v.size(); ++i)
        cin >> v[i];
}

int main() {
    CakeSharing cs;
    int H;
    cin >> H;
    vector<string> roses(H);
    getVector(roses);
    int NP;
    cin >> NP;

    vector<string> ret = cs.cut(roses, NP);
    cout << ret.size() << endl;
    for (int i = 0; i < (int)ret.size(); ++i)
        cout << ret[i] << endl;
    cout.flush();
}
