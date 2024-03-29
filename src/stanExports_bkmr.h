// Generated by rstantools.  Do not edit by hand.

/*
    bkmr2 is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    bkmr2 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with bkmr2.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.21.0
#include <stan/model/model_header.hpp>
namespace model_bkmr_namespace {
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
    reader.add_event(0, 0, "start", "model_bkmr");
    reader.add_event(51, 49, "end", "model_bkmr");
    return reader;
}
#include <stan_meta_header.hpp>
class model_bkmr
  : public stan::model::model_base_crtp<model_bkmr> {
private:
        int N;
        int p;
        int m;
        int est_phi;
        double phi_a;
        double phi_b;
        double sigma_a;
        double sigma_b;
        std::vector<int> y;
        std::vector<int> n;
        matrix_d Q;
        matrix_d R_inv;
        matrix_d M;
        matrix_d P;
public:
    model_bkmr(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, 0, pstream__);
    }
    model_bkmr(stan::io::var_context& context__,
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
        static const char* function__ = "model_bkmr_namespace::model_bkmr";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        try {
            // initialize data block variables from context__
            current_statement_begin__ = 8;
            context__.validate_dims("data initialization", "N", "int", context__.to_vec());
            N = int(0);
            vals_i__ = context__.vals_i("N");
            pos__ = 0;
            N = vals_i__[pos__++];
            check_greater_or_equal(function__, "N", N, 0);
            current_statement_begin__ = 9;
            context__.validate_dims("data initialization", "p", "int", context__.to_vec());
            p = int(0);
            vals_i__ = context__.vals_i("p");
            pos__ = 0;
            p = vals_i__[pos__++];
            check_greater_or_equal(function__, "p", p, 1);
            current_statement_begin__ = 10;
            context__.validate_dims("data initialization", "m", "int", context__.to_vec());
            m = int(0);
            vals_i__ = context__.vals_i("m");
            pos__ = 0;
            m = vals_i__[pos__++];
            check_greater_or_equal(function__, "m", m, 1);
            current_statement_begin__ = 11;
            context__.validate_dims("data initialization", "est_phi", "int", context__.to_vec());
            est_phi = int(0);
            vals_i__ = context__.vals_i("est_phi");
            pos__ = 0;
            est_phi = vals_i__[pos__++];
            check_greater_or_equal(function__, "est_phi", est_phi, 0);
            check_less_or_equal(function__, "est_phi", est_phi, 1);
            current_statement_begin__ = 12;
            context__.validate_dims("data initialization", "phi_a", "double", context__.to_vec());
            phi_a = double(0);
            vals_r__ = context__.vals_r("phi_a");
            pos__ = 0;
            phi_a = vals_r__[pos__++];
            check_greater_or_equal(function__, "phi_a", phi_a, 0);
            current_statement_begin__ = 13;
            context__.validate_dims("data initialization", "phi_b", "double", context__.to_vec());
            phi_b = double(0);
            vals_r__ = context__.vals_r("phi_b");
            pos__ = 0;
            phi_b = vals_r__[pos__++];
            check_greater_or_equal(function__, "phi_b", phi_b, 0);
            current_statement_begin__ = 14;
            context__.validate_dims("data initialization", "sigma_a", "double", context__.to_vec());
            sigma_a = double(0);
            vals_r__ = context__.vals_r("sigma_a");
            pos__ = 0;
            sigma_a = vals_r__[pos__++];
            check_greater_or_equal(function__, "sigma_a", sigma_a, 0);
            current_statement_begin__ = 15;
            context__.validate_dims("data initialization", "sigma_b", "double", context__.to_vec());
            sigma_b = double(0);
            vals_r__ = context__.vals_r("sigma_b");
            pos__ = 0;
            sigma_b = vals_r__[pos__++];
            check_greater_or_equal(function__, "sigma_b", sigma_b, 0);
            current_statement_begin__ = 16;
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
            }
            current_statement_begin__ = 17;
            validate_non_negative_index("n", "N", N);
            context__.validate_dims("data initialization", "n", "int", context__.to_vec(N));
            n = std::vector<int>(N, int(0));
            vals_i__ = context__.vals_i("n");
            pos__ = 0;
            size_t n_k_0_max__ = N;
            for (size_t k_0__ = 0; k_0__ < n_k_0_max__; ++k_0__) {
                n[k_0__] = vals_i__[pos__++];
            }
            size_t n_i_0_max__ = N;
            for (size_t i_0__ = 0; i_0__ < n_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "n[i_0__]", n[i_0__], 1);
            }
            current_statement_begin__ = 18;
            validate_non_negative_index("Q", "N", N);
            validate_non_negative_index("Q", "p", p);
            context__.validate_dims("data initialization", "Q", "matrix_d", context__.to_vec(N,p));
            Q = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(N, p);
            vals_r__ = context__.vals_r("Q");
            pos__ = 0;
            size_t Q_j_2_max__ = p;
            size_t Q_j_1_max__ = N;
            for (size_t j_2__ = 0; j_2__ < Q_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < Q_j_1_max__; ++j_1__) {
                    Q(j_1__, j_2__) = vals_r__[pos__++];
                }
            }
            current_statement_begin__ = 19;
            validate_non_negative_index("R_inv", "p", p);
            validate_non_negative_index("R_inv", "p", p);
            context__.validate_dims("data initialization", "R_inv", "matrix_d", context__.to_vec(p,p));
            R_inv = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(p, p);
            vals_r__ = context__.vals_r("R_inv");
            pos__ = 0;
            size_t R_inv_j_2_max__ = p;
            size_t R_inv_j_1_max__ = p;
            for (size_t j_2__ = 0; j_2__ < R_inv_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < R_inv_j_1_max__; ++j_1__) {
                    R_inv(j_1__, j_2__) = vals_r__[pos__++];
                }
            }
            current_statement_begin__ = 20;
            validate_non_negative_index("M", "N", N);
            validate_non_negative_index("M", "m", m);
            context__.validate_dims("data initialization", "M", "matrix_d", context__.to_vec(N,m));
            M = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(N, m);
            vals_r__ = context__.vals_r("M");
            pos__ = 0;
            size_t M_j_2_max__ = m;
            size_t M_j_1_max__ = N;
            for (size_t j_2__ = 0; j_2__ < M_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < M_j_1_max__; ++j_1__) {
                    M(j_1__, j_2__) = vals_r__[pos__++];
                }
            }
            current_statement_begin__ = 21;
            validate_non_negative_index("P", "m", m);
            validate_non_negative_index("P", "m", m);
            context__.validate_dims("data initialization", "P", "matrix_d", context__.to_vec(m,m));
            P = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(m, m);
            vals_r__ = context__.vals_r("P");
            pos__ = 0;
            size_t P_j_2_max__ = m;
            size_t P_j_1_max__ = m;
            for (size_t j_2__ = 0; j_2__ < P_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < P_j_1_max__; ++j_1__) {
                    P(j_1__, j_2__) = vals_r__[pos__++];
                }
            }
            // initialize transformed data variables
            // execute transformed data statements
            // validate transformed data
            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 24;
            validate_non_negative_index("beta_", "p", p);
            num_params_r__ += p;
            current_statement_begin__ = 25;
            validate_non_negative_index("h", "m", m);
            num_params_r__ += m;
            current_statement_begin__ = 26;
            validate_non_negative_index("phi", "est_phi", est_phi);
            num_params_r__ += (1 * est_phi);
            current_statement_begin__ = 27;
            num_params_r__ += 1;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    ~model_bkmr() { }
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
        current_statement_begin__ = 24;
        if (!(context__.contains_r("beta_")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable beta_ missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("beta_");
        pos__ = 0U;
        validate_non_negative_index("beta_", "p", p);
        context__.validate_dims("parameter initialization", "beta_", "vector_d", context__.to_vec(p));
        Eigen::Matrix<double, Eigen::Dynamic, 1> beta_(p);
        size_t beta__j_1_max__ = p;
        for (size_t j_1__ = 0; j_1__ < beta__j_1_max__; ++j_1__) {
            beta_(j_1__) = vals_r__[pos__++];
        }
        try {
            writer__.vector_unconstrain(beta_);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable beta_: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 25;
        if (!(context__.contains_r("h")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable h missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("h");
        pos__ = 0U;
        validate_non_negative_index("h", "m", m);
        context__.validate_dims("parameter initialization", "h", "vector_d", context__.to_vec(m));
        Eigen::Matrix<double, Eigen::Dynamic, 1> h(m);
        size_t h_j_1_max__ = m;
        for (size_t j_1__ = 0; j_1__ < h_j_1_max__; ++j_1__) {
            h(j_1__) = vals_r__[pos__++];
        }
        try {
            writer__.vector_unconstrain(h);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable h: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 26;
        if (!(context__.contains_r("phi")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable phi missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("phi");
        pos__ = 0U;
        validate_non_negative_index("phi", "est_phi", est_phi);
        context__.validate_dims("parameter initialization", "phi", "double", context__.to_vec(est_phi));
        std::vector<double> phi(est_phi, double(0));
        size_t phi_k_0_max__ = est_phi;
        for (size_t k_0__ = 0; k_0__ < phi_k_0_max__; ++k_0__) {
            phi[k_0__] = vals_r__[pos__++];
        }
        size_t phi_i_0_max__ = est_phi;
        for (size_t i_0__ = 0; i_0__ < phi_i_0_max__; ++i_0__) {
            try {
                writer__.scalar_lb_unconstrain(0, phi[i_0__]);
            } catch (const std::exception& e) {
                stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable phi: ") + e.what()), current_statement_begin__, prog_reader__());
            }
        }
        current_statement_begin__ = 27;
        if (!(context__.contains_r("sigma")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable sigma missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("sigma");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "sigma", "double", context__.to_vec());
        double sigma(0);
        sigma = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0, sigma);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable sigma: ") + e.what()), current_statement_begin__, prog_reader__());
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
            current_statement_begin__ = 24;
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> beta_;
            (void) beta_;  // dummy to suppress unused var warning
            if (jacobian__)
                beta_ = in__.vector_constrain(p, lp__);
            else
                beta_ = in__.vector_constrain(p);
            current_statement_begin__ = 25;
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> h;
            (void) h;  // dummy to suppress unused var warning
            if (jacobian__)
                h = in__.vector_constrain(m, lp__);
            else
                h = in__.vector_constrain(m);
            current_statement_begin__ = 26;
            std::vector<local_scalar_t__> phi;
            size_t phi_d_0_max__ = est_phi;
            phi.reserve(phi_d_0_max__);
            for (size_t d_0__ = 0; d_0__ < phi_d_0_max__; ++d_0__) {
                if (jacobian__)
                    phi.push_back(in__.scalar_lb_constrain(0, lp__));
                else
                    phi.push_back(in__.scalar_lb_constrain(0));
            }
            current_statement_begin__ = 27;
            local_scalar_t__ sigma;
            (void) sigma;  // dummy to suppress unused var warning
            if (jacobian__)
                sigma = in__.scalar_lb_constrain(0, lp__);
            else
                sigma = in__.scalar_lb_constrain(0);
            // transformed parameters
            current_statement_begin__ = 31;
            validate_non_negative_index("beta", "p", p);
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> beta(p);
            stan::math::initialize(beta, DUMMY_VAR__);
            stan::math::fill(beta, DUMMY_VAR__);
            stan::math::assign(beta,multiply(R_inv, beta_));
            // validate transformed parameters
            const char* function__ = "validate transformed params";
            (void) function__;  // dummy to suppress unused var warning
            current_statement_begin__ = 31;
            size_t beta_j_1_max__ = p;
            for (size_t j_1__ = 0; j_1__ < beta_j_1_max__; ++j_1__) {
                if (stan::math::is_uninitialized(beta(j_1__))) {
                    std::stringstream msg__;
                    msg__ << "Undefined transformed parameter: beta" << "(" << j_1__ << ")";
                    stan::lang::rethrow_located(std::runtime_error(std::string("Error initializing variable beta: ") + msg__.str()), current_statement_begin__, prog_reader__());
                }
            }
            // model body
            current_statement_begin__ = 35;
            if (as_bool(logical_eq(est_phi, 1))) {
                current_statement_begin__ = 36;
                lp_accum__.add(multi_normal_log<propto__>(h, rep_vector(0, m), multiply(pow(sigma, 2), stan::math::exp(divide(minus(P), pow(get_base1(phi, est_phi, "phi", 1), 2))))));
                current_statement_begin__ = 37;
                lp_accum__.add(normal_log<propto__>(phi, phi_a, phi_b));
            } else {
                current_statement_begin__ = 40;
                lp_accum__.add(multi_normal_log<propto__>(h, rep_vector(0, m), multiply(pow(sigma, 2), stan::math::exp(minus(P)))));
            }
            current_statement_begin__ = 43;
            lp_accum__.add(binomial_logit_log<propto__>(y, n, add(multiply(Q, beta_), multiply(M, h))));
            current_statement_begin__ = 45;
            lp_accum__.add(normal_log<propto__>(sigma, sigma_a, sigma_b));
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
        names__.push_back("beta_");
        names__.push_back("h");
        names__.push_back("phi");
        names__.push_back("sigma");
        names__.push_back("beta");
        names__.push_back("eta_hat");
    }
    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dims__.push_back(p);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(m);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(est_phi);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(p);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(N);
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
        static const char* function__ = "model_bkmr_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        Eigen::Matrix<double, Eigen::Dynamic, 1> beta_ = in__.vector_constrain(p);
        size_t beta__j_1_max__ = p;
        for (size_t j_1__ = 0; j_1__ < beta__j_1_max__; ++j_1__) {
            vars__.push_back(beta_(j_1__));
        }
        Eigen::Matrix<double, Eigen::Dynamic, 1> h = in__.vector_constrain(m);
        size_t h_j_1_max__ = m;
        for (size_t j_1__ = 0; j_1__ < h_j_1_max__; ++j_1__) {
            vars__.push_back(h(j_1__));
        }
        std::vector<double> phi;
        size_t phi_d_0_max__ = est_phi;
        phi.reserve(phi_d_0_max__);
        for (size_t d_0__ = 0; d_0__ < phi_d_0_max__; ++d_0__) {
            phi.push_back(in__.scalar_lb_constrain(0));
        }
        size_t phi_k_0_max__ = est_phi;
        for (size_t k_0__ = 0; k_0__ < phi_k_0_max__; ++k_0__) {
            vars__.push_back(phi[k_0__]);
        }
        double sigma = in__.scalar_lb_constrain(0);
        vars__.push_back(sigma);
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        if (!include_tparams__ && !include_gqs__) return;
        try {
            // declare and define transformed parameters
            current_statement_begin__ = 31;
            validate_non_negative_index("beta", "p", p);
            Eigen::Matrix<double, Eigen::Dynamic, 1> beta(p);
            stan::math::initialize(beta, DUMMY_VAR__);
            stan::math::fill(beta, DUMMY_VAR__);
            stan::math::assign(beta,multiply(R_inv, beta_));
            if (!include_gqs__ && !include_tparams__) return;
            // validate transformed parameters
            const char* function__ = "validate transformed params";
            (void) function__;  // dummy to suppress unused var warning
            // write transformed parameters
            if (include_tparams__) {
                size_t beta_j_1_max__ = p;
                for (size_t j_1__ = 0; j_1__ < beta_j_1_max__; ++j_1__) {
                    vars__.push_back(beta(j_1__));
                }
            }
            if (!include_gqs__) return;
            // declare and define generated quantities
            current_statement_begin__ = 48;
            validate_non_negative_index("eta_hat", "N", N);
            Eigen::Matrix<double, Eigen::Dynamic, 1> eta_hat(N);
            stan::math::initialize(eta_hat, DUMMY_VAR__);
            stan::math::fill(eta_hat, DUMMY_VAR__);
            stan::math::assign(eta_hat,inv_logit(add(multiply(Q, beta_), multiply(M, h))));
            // validate, write generated quantities
            current_statement_begin__ = 48;
            size_t eta_hat_j_1_max__ = N;
            for (size_t j_1__ = 0; j_1__ < eta_hat_j_1_max__; ++j_1__) {
                vars__.push_back(eta_hat(j_1__));
            }
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
        return "model_bkmr";
    }
    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        size_t beta__j_1_max__ = p;
        for (size_t j_1__ = 0; j_1__ < beta__j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "beta_" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t h_j_1_max__ = m;
        for (size_t j_1__ = 0; j_1__ < h_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "h" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t phi_k_0_max__ = est_phi;
        for (size_t k_0__ = 0; k_0__ < phi_k_0_max__; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "phi" << '.' << k_0__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
            size_t beta_j_1_max__ = p;
            for (size_t j_1__ = 0; j_1__ < beta_j_1_max__; ++j_1__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "beta" << '.' << j_1__ + 1;
                param_names__.push_back(param_name_stream__.str());
            }
        }
        if (!include_gqs__) return;
        size_t eta_hat_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < eta_hat_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "eta_hat" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
    }
    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        size_t beta__j_1_max__ = p;
        for (size_t j_1__ = 0; j_1__ < beta__j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "beta_" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t h_j_1_max__ = m;
        for (size_t j_1__ = 0; j_1__ < h_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "h" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t phi_k_0_max__ = est_phi;
        for (size_t k_0__ = 0; k_0__ < phi_k_0_max__; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "phi" << '.' << k_0__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
            size_t beta_j_1_max__ = p;
            for (size_t j_1__ = 0; j_1__ < beta_j_1_max__; ++j_1__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "beta" << '.' << j_1__ + 1;
                param_names__.push_back(param_name_stream__.str());
            }
        }
        if (!include_gqs__) return;
        size_t eta_hat_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < eta_hat_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "eta_hat" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
    }
}; // model
}  // namespace
typedef model_bkmr_namespace::model_bkmr stan_model;
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
