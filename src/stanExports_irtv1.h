// Generated by rstantools.  Do not edit by hand.

#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.21.0
#include <stan/model/model_header.hpp>
namespace model_irtv1_namespace {
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;
static int current_statement_begin__;
stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_irtv1");
    reader.add_event(32, 30, "end", "model_irtv1");
    return reader;
}
#include <stan_meta_header.hpp>
class model_irtv1
  : public stan::model::model_base_crtp<model_irtv1> {
private:
        int J;
        int K;
        int N;
        std::vector<int> person;
        std::vector<int> item;
        std::vector<int> y;
public:
    model_irtv1(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, 0, pstream__);
    }
    model_irtv1(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, random_seed__, pstream__);
    }
    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        typedef double local_scalar_t__;
        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning
        current_statement_begin__ = -1;
        static const char* function__ = "model_irtv1_namespace::model_irtv1";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        try {
            // initialize data block variables from context__
            current_statement_begin__ = 2;
            context__.validate_dims("data initialization", "J", "int", context__.to_vec());
            J = int(0);
            vals_i__ = context__.vals_i("J");
            pos__ = 0;
            J = vals_i__[pos__++];
            check_greater_or_equal(function__, "J", J, 1);
            current_statement_begin__ = 3;
            context__.validate_dims("data initialization", "K", "int", context__.to_vec());
            K = int(0);
            vals_i__ = context__.vals_i("K");
            pos__ = 0;
            K = vals_i__[pos__++];
            check_greater_or_equal(function__, "K", K, 1);
            current_statement_begin__ = 4;
            context__.validate_dims("data initialization", "N", "int", context__.to_vec());
            N = int(0);
            vals_i__ = context__.vals_i("N");
            pos__ = 0;
            N = vals_i__[pos__++];
            check_greater_or_equal(function__, "N", N, 1);
            current_statement_begin__ = 6;
            validate_non_negative_index("person", "N", N);
            context__.validate_dims("data initialization", "person", "int", context__.to_vec(N));
            person = std::vector<int>(N, int(0));
            vals_i__ = context__.vals_i("person");
            pos__ = 0;
            size_t person_k_0_max__ = N;
            for (size_t k_0__ = 0; k_0__ < person_k_0_max__; ++k_0__) {
                person[k_0__] = vals_i__[pos__++];
            }
            size_t person_i_0_max__ = N;
            for (size_t i_0__ = 0; i_0__ < person_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "person[i_0__]", person[i_0__], 1);
                check_less_or_equal(function__, "person[i_0__]", person[i_0__], J);
            }
            current_statement_begin__ = 7;
            validate_non_negative_index("item", "N", N);
            context__.validate_dims("data initialization", "item", "int", context__.to_vec(N));
            item = std::vector<int>(N, int(0));
            vals_i__ = context__.vals_i("item");
            pos__ = 0;
            size_t item_k_0_max__ = N;
            for (size_t k_0__ = 0; k_0__ < item_k_0_max__; ++k_0__) {
                item[k_0__] = vals_i__[pos__++];
            }
            size_t item_i_0_max__ = N;
            for (size_t i_0__ = 0; i_0__ < item_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "item[i_0__]", item[i_0__], 1);
                check_less_or_equal(function__, "item[i_0__]", item[i_0__], K);
            }
            current_statement_begin__ = 8;
            validate_non_negative_index("y", "N", N);
            context__.validate_dims("data initialization", "y", "int", context__.to_vec(N));
            y = std::vector<int>(N, int(0));
            vals_i__ = context__.vals_i("y");
            pos__ = 0;
            size_t y_k_0_max__ = N;
            for (size_t k_0__ = 0; k_0__ < y_k_0_max__; ++k_0__) {
                y[k_0__] = vals_i__[pos__++];
            }
            size_t y_i_0_max__ = N;
            for (size_t i_0__ = 0; i_0__ < y_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "y[i_0__]", y[i_0__], 0);
                check_less_or_equal(function__, "y[i_0__]", y[i_0__], 1);
            }
            // initialize transformed data variables
            // execute transformed data statements
            // validate transformed data
            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 12;
            validate_non_negative_index("gamma", "K", K);
            num_params_r__ += K;
            current_statement_begin__ = 13;
            validate_non_negative_index("alpha", "J", J);
            num_params_r__ += J;
            current_statement_begin__ = 14;
            validate_non_negative_index("beta", "K", K);
            num_params_r__ += K;
            current_statement_begin__ = 15;
            num_params_r__ += 1;
            current_statement_begin__ = 16;
            num_params_r__ += 1;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    ~model_irtv1() { }
    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        typedef double local_scalar_t__;
        stan::io::writer<double> writer__(params_r__, params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;
        current_statement_begin__ = 12;
        if (!(context__.contains_r("gamma")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable gamma missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("gamma");
        pos__ = 0U;
        validate_non_negative_index("gamma", "K", K);
        context__.validate_dims("parameter initialization", "gamma", "vector_d", context__.to_vec(K));
        Eigen::Matrix<double, Eigen::Dynamic, 1> gamma(K);
        size_t gamma_j_1_max__ = K;
        for (size_t j_1__ = 0; j_1__ < gamma_j_1_max__; ++j_1__) {
            gamma(j_1__) = vals_r__[pos__++];
        }
        try {
            writer__.vector_lb_unconstrain(0, gamma);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable gamma: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 13;
        if (!(context__.contains_r("alpha")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable alpha missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("alpha");
        pos__ = 0U;
        validate_non_negative_index("alpha", "J", J);
        context__.validate_dims("parameter initialization", "alpha", "vector_d", context__.to_vec(J));
        Eigen::Matrix<double, Eigen::Dynamic, 1> alpha(J);
        size_t alpha_j_1_max__ = J;
        for (size_t j_1__ = 0; j_1__ < alpha_j_1_max__; ++j_1__) {
            alpha(j_1__) = vals_r__[pos__++];
        }
        try {
            writer__.vector_unconstrain(alpha);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable alpha: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 14;
        if (!(context__.contains_r("beta")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable beta missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("beta");
        pos__ = 0U;
        validate_non_negative_index("beta", "K", K);
        context__.validate_dims("parameter initialization", "beta", "vector_d", context__.to_vec(K));
        Eigen::Matrix<double, Eigen::Dynamic, 1> beta(K);
        size_t beta_j_1_max__ = K;
        for (size_t j_1__ = 0; j_1__ < beta_j_1_max__; ++j_1__) {
            beta(j_1__) = vals_r__[pos__++];
        }
        try {
            writer__.vector_unconstrain(beta);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable beta: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 15;
        if (!(context__.contains_r("sigma_beta")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable sigma_beta missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("sigma_beta");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "sigma_beta", "double", context__.to_vec());
        double sigma_beta(0);
        sigma_beta = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0, sigma_beta);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable sigma_beta: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 16;
        if (!(context__.contains_r("sigma_gamma")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable sigma_gamma missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("sigma_gamma");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "sigma_gamma", "double", context__.to_vec());
        double sigma_gamma(0);
        sigma_gamma = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0, sigma_gamma);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable sigma_gamma: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }
    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }
    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(std::vector<T__>& params_r__,
                 std::vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {
        typedef T__ local_scalar_t__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // dummy to suppress unused var warning
        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;
        try {
            stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
            // model parameters
            current_statement_begin__ = 12;
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> gamma;
            (void) gamma;  // dummy to suppress unused var warning
            if (jacobian__)
                gamma = in__.vector_lb_constrain(0, K, lp__);
            else
                gamma = in__.vector_lb_constrain(0, K);
            current_statement_begin__ = 13;
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> alpha;
            (void) alpha;  // dummy to suppress unused var warning
            if (jacobian__)
                alpha = in__.vector_constrain(J, lp__);
            else
                alpha = in__.vector_constrain(J);
            current_statement_begin__ = 14;
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> beta;
            (void) beta;  // dummy to suppress unused var warning
            if (jacobian__)
                beta = in__.vector_constrain(K, lp__);
            else
                beta = in__.vector_constrain(K);
            current_statement_begin__ = 15;
            local_scalar_t__ sigma_beta;
            (void) sigma_beta;  // dummy to suppress unused var warning
            if (jacobian__)
                sigma_beta = in__.scalar_lb_constrain(0, lp__);
            else
                sigma_beta = in__.scalar_lb_constrain(0);
            current_statement_begin__ = 16;
            local_scalar_t__ sigma_gamma;
            (void) sigma_gamma;  // dummy to suppress unused var warning
            if (jacobian__)
                sigma_gamma = in__.scalar_lb_constrain(0, lp__);
            else
                sigma_gamma = in__.scalar_lb_constrain(0);
            // model body
            current_statement_begin__ = 21;
            lp_accum__.add(std_normal_log<propto__>(alpha));
            current_statement_begin__ = 22;
            lp_accum__.add(normal_log<propto__>(beta, 0, sigma_beta));
            current_statement_begin__ = 23;
            lp_accum__.add(normal_log<propto__>(gamma, 0, sigma_gamma));
            current_statement_begin__ = 24;
            lp_accum__.add(cauchy_log<propto__>(sigma_beta, 0, 5));
            current_statement_begin__ = 25;
            lp_accum__.add(cauchy_log<propto__>(sigma_gamma, 0, 5));
            current_statement_begin__ = 27;
            lp_accum__.add(bernoulli_logit_log<propto__>(y, elt_multiply(stan::model::rvalue(gamma, stan::model::cons_list(stan::model::index_multi(item), stan::model::nil_index_list()), "gamma"), subtract(stan::model::rvalue(alpha, stan::model::cons_list(stan::model::index_multi(person), stan::model::nil_index_list()), "alpha"), stan::model::rvalue(beta, stan::model::cons_list(stan::model::index_multi(item), stan::model::nil_index_list()), "beta")))));
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
        lp_accum__.add(lp__);
        return lp_accum__.sum();
    } // log_prob()
    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }
    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("gamma");
        names__.push_back("alpha");
        names__.push_back("beta");
        names__.push_back("sigma_beta");
        names__.push_back("sigma_gamma");
    }
    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dims__.push_back(K);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(J);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(K);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
    }
    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        typedef double local_scalar_t__;
        vars__.resize(0);
        stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
        static const char* function__ = "model_irtv1_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        Eigen::Matrix<double, Eigen::Dynamic, 1> gamma = in__.vector_lb_constrain(0, K);
        size_t gamma_j_1_max__ = K;
        for (size_t j_1__ = 0; j_1__ < gamma_j_1_max__; ++j_1__) {
            vars__.push_back(gamma(j_1__));
        }
        Eigen::Matrix<double, Eigen::Dynamic, 1> alpha = in__.vector_constrain(J);
        size_t alpha_j_1_max__ = J;
        for (size_t j_1__ = 0; j_1__ < alpha_j_1_max__; ++j_1__) {
            vars__.push_back(alpha(j_1__));
        }
        Eigen::Matrix<double, Eigen::Dynamic, 1> beta = in__.vector_constrain(K);
        size_t beta_j_1_max__ = K;
        for (size_t j_1__ = 0; j_1__ < beta_j_1_max__; ++j_1__) {
            vars__.push_back(beta(j_1__));
        }
        double sigma_beta = in__.scalar_lb_constrain(0);
        vars__.push_back(sigma_beta);
        double sigma_gamma = in__.scalar_lb_constrain(0);
        vars__.push_back(sigma_gamma);
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        if (!include_tparams__ && !include_gqs__) return;
        try {
            if (!include_gqs__ && !include_tparams__) return;
            if (!include_gqs__) return;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng, params_r_vec, params_i_vec, vars_vec, include_tparams, include_gqs, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }
    std::string model_name() const {
        return "model_irtv1";
    }
    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        size_t gamma_j_1_max__ = K;
        for (size_t j_1__ = 0; j_1__ < gamma_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "gamma" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t alpha_j_1_max__ = J;
        for (size_t j_1__ = 0; j_1__ < alpha_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "alpha" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t beta_j_1_max__ = K;
        for (size_t j_1__ = 0; j_1__ < beta_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "beta" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_beta";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_gamma";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
        }
        if (!include_gqs__) return;
    }
    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        size_t gamma_j_1_max__ = K;
        for (size_t j_1__ = 0; j_1__ < gamma_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "gamma" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t alpha_j_1_max__ = J;
        for (size_t j_1__ = 0; j_1__ < alpha_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "alpha" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t beta_j_1_max__ = K;
        for (size_t j_1__ = 0; j_1__ < beta_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "beta" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_beta";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_gamma";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
        }
        if (!include_gqs__) return;
    }
}; // model
}  // namespace
typedef model_irtv1_namespace::model_irtv1 stan_model;
#ifndef USING_R
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
#endif
#endif
