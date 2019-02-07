#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

//g++ --std=c++0x -W -Wall -Wno-sign-compare -O2 -s -pipe -mmmx -msse -msse2 -msse3 ./WorldCupLineup.cpp

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
	inline double nextDouble() {return nextInt()*iDouble;}
} rnd;

double timeLimit=9.92;
//const int64_t CYCLES_PER_SEC=2800000000;
const int64_t CYCLES_PER_SEC=3600000000;
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

class WorldCupLineup {
public:
    Timer timer;
    int N,M,num_player;
    int8_t cur_player[10][22],playerId[31];
    int atk[31],def[31];double agg[31];
    int g_atk[51],g_def[51];double g_agg[51];
    double n_agg[10];
    int8_t n_card[31];
    vector<int> group_member[51];
    char spt;
    vector<string> ans;
    int spF,spD;
    int8_t now_part[10],lineup[10];
    bool usedp[31];
    double injury_rate;
    int bestF,bestD;
    int8_t best_player[10][22],bestplayerId[31];
    double best_score,cur_score,pre_score;
    int8_t lineup_id[31];
    int groupbit[51],pow2[31];
    //double param_factor;
    //double basebias[10];
    int first_score=0;
    double first_agg[10];
    int simu_one(int ii){
        int score=0;
        int s_atk=0,s_def=0;
        double aggfac=1.2;
        memset(now_part,0,sizeof(now_part));
        memset(usedp,0,sizeof(usedp));
        memset(n_card,0,sizeof(n_card));
        //memset(lineup_id,-1,sizeof(lineup_id));
        int lineupbit=0;
        rep(i,10){
            int id=playerId[cur_player[i][now_part[i]]];
            usedp[id]=true;
            lineup[i]=id;
            lineup_id[id]=i;
            lineupbit|=pow2[id];
            if(ii==0){
                if(i<=spF) s_atk+=atk[id]*2;
                else if(spD<i) s_def+=def[id]*2;
                else s_atk+=atk[id],s_def+=def[id];
                n_agg[i]=agg[id]*aggfac;
            }
        }
        if(ii==0)rep(i,M)if((groupbit[i]&lineupbit)==groupbit[i]){
            rep(j,group_member[i].size()){
                int id=group_member[i][j];
                int k=lineup_id[id];
                if(k<=spF) s_atk+=g_atk[i]*2;
                else if(spD<k) s_def+=g_def[i]*2;
                else s_atk+=g_atk[i],s_def+=g_def[i];
                n_agg[k]+=g_agg[i]*aggfac;
            }
        }
        
        if(ii==0) memcpy(first_agg,n_agg,sizeof(n_agg));
        else memcpy(n_agg,first_agg,sizeof(first_agg));
        
        rep(i,10){
            if(rnd.nextDouble()<injury_rate||(rnd.nextInt(100)<n_agg[i]&&++n_card[lineup[i]]==2)){
                int id;
                ++now_part[i];//lineup_id[lineup[i]]=-1;
                while(id=playerId[cur_player[i][now_part[i]]],usedp[id]&&now_part[i]<21) ++now_part[i];
                usedp[id]=true;//lineup_id[id]=i;
            }
        }
        if(ii==0)score=min(s_atk,s_def),first_score=score;
        else score=first_score;
        rep(count,4){
            //memset(lineup_id,-1,sizeof(lineup_id));
            s_atk=0,s_def=0;
            lineupbit=0;
            rep(i,10){
                int id=playerId[cur_player[i][now_part[i]]];
                //usedp[id]=true;
                lineup[i]=id;
                lineupbit|=pow2[id];
                lineup_id[id]=i;
                if(i<=spF) s_atk+=atk[id]*2;
                else if(spD<i) s_def+=def[id]*2;
                else s_atk+=atk[id],s_def+=def[id];
                n_agg[i]=agg[id]*aggfac;
            }
            rep(i,M)if((groupbit[i]&lineupbit)==groupbit[i]){
                rep(j,group_member[i].size()){
                    int id=group_member[i][j];
                    int k=lineup_id[id];
                    if(k<=spF) s_atk+=g_atk[i]*2;
                    else if(spD<k) s_def+=g_def[i]*2;
                    else s_atk+=g_atk[i],s_def+=g_def[i];
                    n_agg[k]+=g_agg[i]*aggfac;
                }
            }
            rep(i,10){
                if(rnd.nextDouble()<injury_rate||(rnd.nextInt(100)<n_agg[i]&&++n_card[lineup[i]]==2)){
                    int id;
                    ++now_part[i];//lineup_id[lineup[i]]=-1;
                    while(id=playerId[cur_player[i][now_part[i]]],usedp[id]&&now_part[i]<21) ++now_part[i];
                    usedp[id]=true;//lineup_id[id]=i;
                }
            }
            score+=min(s_atk,s_def);
        }
        return score;
    }
    double get_ave_score(double temp){
        double score=0;
        int num_simu=50,ckp=3,ckp2=19;
        rep(i,num_simu){
            score+=simu_one(i);
            if(i==ckp){
                double ret=score/(ckp+1);
                if((ret-pre_score)*temp<-10) return ret;
            }
            else if(i==ckp2){
                double ret=score/(ckp2+1);
                if((ret-pre_score)*temp<-6) return ret;
            }
        }
        return score/num_simu;
    }
    void update_best(double cur){
        best_score=cur;
        memcpy(best_player,cur_player,sizeof(cur_player));
        memcpy(bestplayerId,playerId,sizeof(playerId));
        bestF=spF,bestD=spD;
        //cerr<<timer.get()<<" "<<best_score<<endl;
    }
    inline bool forceupdate(double sub,double temp){
        if(sub>=0) return true;
        double d=(sub)*temp;
        if(d<-6) return false;
        return exp(d)>rnd.nextDouble();
    }
    void improve_formation(){
        double starttemp=40,endtemp=5,curtemp;
        int turn=0;
        double invtl=1.0/timeLimit;
        spF=4,spD=4;
        while(1){
            ++turn;
            double t=timer.get();
            if(t>timeLimit){
                cerr<<turn<<endl;
                return;
            }
            pre_score=cur_score;
            //param_factor=(timeLimit-t)*invtl;
            curtemp=1.0/(starttemp+(endtemp-starttemp)*t*invtl);
            if(turn%100==0&&(spD<9||spF<spD)){
                int pre=spD;
                if((rnd.nextInt()&1)&&spD<9) ++spD;
                else if(spF<spD)--spD;
                else continue;
                cur_score=get_ave_score(curtemp);
                if(forceupdate(cur_score-pre_score,curtemp)){
                    if(cur_score>best_score) update_best(cur_score);
                }
                else{
                    spD=pre;
                    cur_score=pre_score;
                }
            }
            else if(turn%50==0&&(spF>-1||spF<spD)){
                int pre=spF;
                if((rnd.nextInt()&1)&&spF>-1) --spF;
                else if(spF<spD)++spF;
                else continue;
                cur_score=get_ave_score(curtemp);
                if(forceupdate(cur_score-pre_score,curtemp)){
                    if(cur_score>best_score) update_best(cur_score);
                }
                else{
                    spF=pre;
                    cur_score=pre_score;
                }
            }
            else{
                if(rnd.nextInt()&1){
                    int tid=rnd.nextInt(9);
                    //if(tid<9){
                        swap(playerId[cur_player[tid][0]],playerId[cur_player[tid+1][0]]);
                        cur_score=get_ave_score(curtemp);
                        if(forceupdate(cur_score-pre_score,curtemp)){
                            if(cur_score>best_score) update_best(cur_score);
                        }
                        else{
                            swap(playerId[cur_player[tid][0]],playerId[cur_player[tid+1][0]]);
                            cur_score=pre_score;
                        }
                    //}
                    /*
                    swap(playerId[tid],playerId[tid+1]);
                    cur_score=get_ave_score();
                    if(forceupdate(cur_score-pre_score,curtemp)){
                        if(cur_score>best_score) update_best(cur_score);
                    }
                    else{
                        swap(playerId[tid],playerId[tid+1]);
                        cur_score=pre_score;
                    }*/
                }
                else{
                    int col=rnd.nextInt(15),row=rnd.nextInt(10);
                    if(col==0){
                        swap(playerId[cur_player[row][0]],playerId[cur_player[row][1]]);
                        cur_score=get_ave_score(curtemp);
                        if(forceupdate(cur_score-pre_score,curtemp)){
                            if(cur_score>best_score) update_best(cur_score);
                        }
                        else{
                            swap(playerId[cur_player[row][0]],playerId[cur_player[row][1]]);
                            cur_score=pre_score;
                        }
                    }
                    else{
                        swap(cur_player[row][col],cur_player[row][col+1]);
                        cur_score=get_ave_score(curtemp);
                        if(forceupdate(cur_score-pre_score,curtemp)){
                            if(cur_score>best_score) update_best(cur_score);
                        }
                        else{
                            swap(cur_player[row][col],cur_player[row][col+1]);
                            cur_score=pre_score;
                        }
                    }
                }
            }
        }
        return;
    }
    void init_formation(){
        rep(i,num_player) cur_player[i][0]=i;
        rep(i,num_player)FOR(j,num_player,31) cur_player[i][j-9]=j;
        vector<pair<pint,int> > sorted_list;
        rep(i,N){
            sorted_list.pb(make_pair(atk[i]+def[i],atk[i]-def[i]),i);
        }
        sort(sorted_list.begin(),sorted_list.end());
        reverse(sorted_list.begin(),sorted_list.end());
        int l=0,r=9;
        rep(i,num_player){
            if(sorted_list[i].first.second>0) playerId[l]=sorted_list[i].second,++l;
            else playerId[r]=sorted_list[i].second,--r;
        }
        FOR(i,num_player,30){
            playerId[i]=sorted_list[i].second;
        }
        playerId[30]=30;
        return;
    }
    vector<string> selectPositions(vector<string> players, vector<string> groups) {
        timer.reset();
        N=30;//players.size();
        M=groups.size();
        num_player=10;
        //injury_rate=rnd.nextDouble()*0.05;
        injury_rate=0.025;
        spD=4,spF=4;
        memset(groupbit,0,sizeof(groupbit));
        rep(i,31) pow2[i]=1<<i;
        rep(i,N){
            stringstream ss(players[i]);
            ss>>atk[i]>>spt>>def[i]>>spt>>agg[i];
        }
        rep(i,M){
            stringstream ss(groups[i]);
            string pat,para;
            ss>>pat>>para;
            stringstream ss1(pat),ss2(para);
            int num;
            while(!ss1.eof()){
                ss1>>num;
                ss1.ignore();
                group_member[i].pb(num);
                groupbit[i]|=pow2[num];
            }
            ss2>>g_atk[i]>>spt>>g_def[i]>>spt>>g_agg[i];
        }
        //param_factor=1.0;
        init_formation();
        pre_score=0;
        cur_score=get_ave_score(0);
        update_best(cur_score);
        improve_formation();
        rep(i,num_player){
            string tmp;
            if(i<=bestF) tmp+="F ";
            else if(i>bestD) tmp+="D ";
            else tmp+="M ";
            rep(j,21){
                tmp+=to_string(bestplayerId[best_player[i][j]]);
                tmp+=',';
            }
            tmp.pop_back();
            ans.pb(tmp);
        }
        //for(auto it:ans) cerr<<it<<endl;
        cerr<<best_score<<endl;
        return ans;
        //return {"F 0,10,11,12", "F 1,10,11,12", "F 2,10,11,12", "M 3,10,11,12", "M 4,10,11,12", "M 5,10,11,12", "M 6,10,11,12", "D 7,10,11,12", "D 8,10,11,12", "D 9,10,11,12"};
    }
};
// -------8<------- end of solution submitted to the website -------8<-------

template<class T> void getVector(vector<T>& v) {
    cin.ignore();
    for (int i = 0; i < v.size(); ++i)
        getline(cin, v[i]);
}

int main() {
    WorldCupLineup sol;
    int H;
    cin >> H;
    vector<string> players(H);
    getVector(players);
    cin >> H;
    vector<string> groups(H);
    getVector(groups);

    vector<string> ret = sol.selectPositions(players, groups);
    cout << ret.size() << endl;
    for (int i = 0; i < (int)ret.size(); ++i)
        cout << ret[i] << endl;
    cout.flush();
}
