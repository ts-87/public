#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

double PI=acos(-1.0);
#define eps (1e-10)
class InvestmentAdvice {
public:
    int nEx,remain;
    int turn=0,num_round=0;
    int ubBet=50;
    double border=-eps;
    double reliable[51],predict_accuracy[51],rely_stdev[51],predict_stdev[51];
    vector<pair<double,int> > pri_list;
    int ret_result[101][51];
    vector<int> ans,pre_ans,pre_advice;
    double advice_list[101][51],recent_list[101][51],pls[51];
    void assign_ans(vector<int> &advice,int Left){
        ans.assign(nEx,0);
        sort(pri_list.begin(),pri_list.end(),greater<pair<double,int> >());
        if(400000*nEx/2.1<remain) ubBet=0;
        if(Left>1)for(int i=nEx-1;i>=0;--i){
            ans[pri_list[i].second]+=ubBet;
            remain-=ubBet;
        }
        rep(i,nEx)if(pri_list[i].first>=border){
            if(remain<=0)break;
            int tmp=400000-ans[pri_list[i].second];
            ans[pri_list[i].second]+=min(tmp,remain);
            remain-=tmp;
        }
        return;
    }
    inline double gaussfunc(double x,double m,double sd){
        return exp(-(x-m)*(x-m)*0.5/(sd*sd))/(sqrt(PI*2)*sd);
    }
    void precalc(vector<int> &recent){
        if(turn==0){
            rep(i,nEx){
                reliable[i]=0.5;
                predict_accuracy[i]=0.5;
                rely_stdev[i]=0.5;
                predict_stdev[i]=0.1;
            }
        }
        else{
            double fail,correct,ratio,sub,pre_stdev_rel;
            rep(i,nEx){
                if(pre_ans[i]==0) pre_ans[i]=1;
                pre_stdev_rel=rely_stdev[i];
                ratio=(double)recent[i]/pre_ans[i];
                recent_list[turn-1][i]=ratio;
                fail=gaussfunc(ratio,0,0.1);
                correct=gaussfunc(ratio,advice_list[turn-1][i]/100,predict_stdev[i]);
                if(correct/fail>1.0){
                    sub=1.0;
                    double tmp=ratio-advice_list[turn-1][i]/100;
                    tmp*=0.35;
                    //predict_stdev[i]=sqrt((pre_stdev_rel*predict_stdev[i]*predict_stdev[i]+sub*tmp*tmp)/(pre_stdev_rel+sub));
                    predict_stdev[i]=max(0.03,sqrt((pre_stdev_rel*predict_stdev[i]*predict_stdev[i]+sub*tmp*tmp)/(pre_stdev_rel+sub)));
                    rely_stdev[i]+=sub;
                }
                double cur=0.5,weight=1.0;
                rep(j,turn){
                    fail=gaussfunc(recent_list[j][i],0,0.1);
                    correct=gaussfunc(recent_list[j][i],advice_list[j][i]/100,predict_stdev[i]);
                    double tmp=correct/(correct+fail);
                    cur+=tmp;
                    weight+=1;
                }
                predict_accuracy[i]=cur/weight;
                cur=0.5,weight=1.0;
                rep(j,turn){
                    fail=gaussfunc(recent_list[j][i],0,0.1);
                    correct=gaussfunc(recent_list[j][i],advice_list[j][i]/100,0.15);
                    double tmp=correct/(correct+fail);
                    cur+=tmp;
                    weight+=1;
                }
                if(correct/fail>1.0&&cur/weight/predict_accuracy[i]>1.3) pls[i]+=0.005;
                predict_accuracy[i]+=pls[i];
                //cerr<<i<<"  "<<cur/weight<<"     "<<predict_accuracy[i]<<"     "<<pls[i]<<endl;
                double bd=max(0.03,0.065-(0.065-0.03)*turn/20);
                
                if(turn<=20&&predict_accuracy[i]>0.5&&predict_stdev[i]<=bd+eps) predict_accuracy[i]*=0.7*predict_accuracy[i]+0.91;
                else if(turn>20&&predict_accuracy[i]>0.4) predict_accuracy[i]*=0.7*predict_accuracy[i]+1.28;
                
            }
        }
        return;
    }
    inline double eval_func(vector<int> &advice,int id){
        int B=2;
        if(turn<B)return (double)advice[id]*(predict_accuracy[id]*turn/B+((double)B-turn)/B*0.5);
        else return (double)advice[id]*(predict_accuracy[id]);
        
    }
    vector<int> getInvestments(vector<int> advice, vector<int> recent, int money, int timeLeft, int roundsLeft) {
        if(turn==0){
            num_round=roundsLeft;
            memset(pls,0,sizeof(pls));
        }
        nEx=advice.size();
        remain=money;
        pri_list.clear();
        
        rep(i,nEx) advice_list[turn][i]=advice[i];
        precalc(recent);
        rep(i,nEx) pri_list.pb(eval_func(advice,i),i);
        assign_ans(advice,roundsLeft);
        /*
        if(roundsLeft==1||true){
            pre_advice.resize(nEx);
            int sum=0;
            rep(i,nEx){
                cerr<<"Ex :"<<i<<"  adv  :"<<pre_advice[i]<<"   recent :  "<<recent[i]*2<<"      acc :"<<predict_accuracy[i]<<"  stdev :"<<predict_stdev[i]*100<<"  pls :"<<pls[i]<<endl;
                sum+=recent[i];
            }
            cerr<<sum<<endl;
            cerr<<endl;
        }
        */
        pre_ans=ans,pre_advice=advice;
        ++turn;
        return ans;
    }
};

// -------8<------- end of solution submitted to the website -------8<-------

template<class T> void getVector(vector<T>& v) {
    for (int i = 0; i < v.size(); ++i)
        cin >> v[i];
}

int main() {
    InvestmentAdvice sol;
	int roundsLeft = 99;
	while (roundsLeft > 1) {
        int A;
        cin >> A;
        vector<int> advice(A);
        getVector(advice);
        int R;
        cin >> R;
        vector<int> recent(R);
        getVector(recent);
        int money;
	    int timeLeft;
	    cin >> money;
	    cin >> timeLeft;
	    cin >> roundsLeft;
        vector<int> ret = sol.getInvestments(advice, recent, money, timeLeft, roundsLeft);
        cout << ret.size() << endl;
        for (int i = 0; i < (int)ret.size(); ++i) cout << ret[i] << endl;
        cout.flush();
	}
}
