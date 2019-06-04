import math

from data_structure import *
from scheduling_util import *
from constants import *
import util


def lerp(a, b, x):
    return a + (b - a) * x


class ProbabilityEstimator(object):

    def __init__(self):
        raise NotImplementedError()

    def set_logger(self, logger):
        self.logger = logger

    def get_success_probability(self, task, variable_time, total_time):
        raise NotImplementedError()


class UtilityEstimator(object):

    def __init__(self):
        raise NotImplementedError()

    def set_logger(self, logger):
        self.logger = logger

    def get_success_utility(self, task):
        raise NotImplementedError()

    def get_session_utility(self, task, duration):
        raise NotImplementedError()


class ExpectedUtilityEstimator(object):

    def __init__(self, probability_estimator, utility_estimator):
        raise NotImplementedError()

    def set_logger(self, logger):
        self.logger = logger

    def initialize(self, tasks, schedule, max_session_duration):
        raise NotImplementedError()

    def get_expected_utility_gain_per_duration(self, task_index, duration):
        raise NotImplementedError()

    def update(self, task_index, on_date, duration):
        raise NotImplementedError()


class SchedulingPolicy(object):

    def __init__(self):
        raise NotImplementedError()

    def set_logger(self, logger):
        self.logger = logger

    def initialize(self, direction, tasks, progress_info, schedule):
        raise NotImplementedError()

    def get_max_session_duration(self):
        raise NotImplementedError()

    def get_next(self, date):
        raise NotImplementedError()

    def get_score(self, task, days_from_today):
        raise NotImplementedError()

    def add(self, task_index, task, task_event):
        raise NotImplementedError()

    def update(self, task_index, date, next_date, duration):
        raise NotImplementedError()

    def delete(self, task_index):
        raise NotImplementedError()


class GaussianProbabilityEstimator(ProbabilityEstimator):

    def __init__(self, variable_time_std, total_time_std, mean_scale):
        self.sqrt2 = math.sqrt(2.0)
        self.variable_time_std = variable_time_std
        self.total_time_std = total_time_std
        self.mean_scale = mean_scale

    def get_success_probability(self, task, variable_time, total_time):
        # task is unused in our estimate

        variable_time = max(0, variable_time)
        total_time = max(0, total_time)

        if variable_time == 0:
            return 1.0

        if total_time == math.inf:
            return 1.0

        x = 0
        mean = (variable_time * self.mean_scale) - total_time
        std = abs(variable_time) * self.variable_time_std + \
            abs(total_time) * self.total_time_std
        if std == 0:
            std = 0.00001

        phi = (1.0 + math.erf((x - mean) / (std * self.sqrt2))) / 2.0
        return phi


class MixedUtilityEstimator(UtilityEstimator):

    def __init__(self):
        pass

    def get_success_utility(self, task):
        # TODO elaborate
        return 1.0 / (1.0 + task.priority.value)

    def get_session_utility(self, task, duration):
        # TODO elaborate
        return 0.0


class GreedyExpectedUtilityEstimator(ExpectedUtilityEstimator):

    def __init__(self, probability_estimator, utility_estimator):
        self.p_estimator = probability_estimator
        self.u_estimator = utility_estimator

    def initialize(self, tasks, progress_info, schedule, max_session_duration, pressure_heuristic):
        # maintain reference to the parameters
        self.tasks = tasks
        self.progress_info = progress_info
        self.schedule = schedule
        self.max_session_duration = max_session_duration
        self.pressure_heuristic = pressure_heuristic

        # sort tasks by deadline
        self.sorted_tasks = [(i, tasks[i]) for i in range(len(tasks))]
        self.sorted_tasks.sort(key=lambda x: x[1].end)

        # compute link from index back to order
        self.index_to_order = [0 for _ in range(len(tasks))]
        for i in range(len(self.sorted_tasks)):
            self.index_to_order[self.sorted_tasks[i][0]] = i

        # initialization
        self.sorted_task_pressure = [0 for _ in range(len(self.sorted_tasks))]
        self.sorted_task_total_time = [
            0 for _ in range(len(self.sorted_tasks))]

        self.update()

    def get_expected_utility_gain_per_duration(self, task_index, duration):
        '''
        Duration is not used here because our estimate is independent of it.
        '''
        task = self.tasks[task_index]
        r = self.progress_info.get_amount_left(task_index)
        s = self.max_session_duration
        i = self.index_to_order[task_index]
        # R = self.sorted_task_pressure[i]
        w = self.sorted_task_total_time[i] - s
        # l = self.pressure_heuristic

        p_good = self.p_estimator.get_success_probability(
            task,
            r - s,
            # lerp(r - s, R - s, l),
            w
        )

        p_bad = self.p_estimator.get_success_probability(
            task,
            r,
            # lerp(r, R, l),
            w
        )

        u = self.u_estimator.get_success_utility(task)
        U = (p_good - p_bad) * u

        # # deprecated heuristic
        # U += self.suffix_sum_cache[task_index]

        U += self.extra_gain_cache[task_index]

        return U / self.max_session_duration

    def update(self, task_index=None, on_date=None, duration=None):
        '''
        Right now we simply re-read from reference to data passed in from initialize()
        '''
        # compute early schedule and total & variable time
        self.start_day = self.schedule.get_schedule_start().add_days(-1)  # -1 is useful
        end_day = self.schedule.get_schedule_end()
        self.schedule_days = self.start_day.days_to(end_day)
        schedule_free_time_until = [0 for _ in range(self.schedule_days)]
        for i in range(1, self.schedule_days):
            schedule_free_time_until[i] = self.schedule.get_free_time(
                self.start_day.add_days(i)) + schedule_free_time_until[i - 1]

        prev_variable_time = 0
        for i in range(len(self.sorted_tasks)):
            variable_time = self.progress_info.get_amount_left(
                self.sorted_tasks[i][0]) + prev_variable_time
            self.sorted_task_pressure[i] = variable_time
            prev_variable_time = variable_time

        for i in range(len(self.sorted_tasks)):
            due = max(0, self.start_day.days_to(self.sorted_tasks[i][1].end))
            if due < self.schedule_days:
                self.sorted_task_total_time[i] = schedule_free_time_until[due]

            else:
                self.sorted_task_total_time[i] = math.inf

        # cache extra gain
        self.extra_gain_cache = [0 for _ in range(len(self.tasks))]
        s = self.max_session_duration
        peak_i = -1
        peak_E = 0
        peak_p_bad = 0
        total_u = 0
        peak_total_u = 0
        for i in range(len(self.sorted_tasks)):
            index, task = self.sorted_tasks[i]
            w = self.sorted_task_total_time[i] - s
            R = self.sorted_task_pressure[i]
            p = self.p_estimator.get_success_probability(
                task,
                R,
                w
            )
            u = self.u_estimator.get_success_utility(task)
            total_u += u
            E = p * u

            if E > peak_E:
                peak_E = E
                peak_i = i
                peak_p_bad = p
                peak_total_u = total_u

        if peak_i > 0:
            peak_w = self.sorted_task_total_time[peak_i] - s
            peak_R = self.sorted_task_pressure[peak_i]
            peak_p_good = self.p_estimator.get_success_probability(
                None,
                peak_R - s,
                peak_w
            )
            peak_u = peak_total_u / (peak_i + 1)
            extra_gain = (peak_p_good - peak_p_bad) * peak_u
            for i in range(peak_i + 1):
                index, _ = self.sorted_tasks[i]
                self.extra_gain_cache[index] = extra_gain

        # # deprecated heuristic
        # # cache suffix sum
        # prev_gain = 0
        # for i in range(len(self.sorted_tasks) - 1, -1, -1):
        #     index, task = self.sorted_tasks[i]
        #     s = self.max_session_duration
        #     w = self.sorted_task_total_time[i] - s
        #     r = self.progress_info.get_amount_left(index)
        #     R = self.sorted_task_pressure[i]
        #     l = self.pressure_heuristic
        #     p_good = self.p_estimator.get_success_probability(
        #         task,
        #         lerp(r, R - s, l),
        #         w
        #     )
        #     p_bad = self.p_estimator.get_success_probability(
        #         task,
        #         lerp(r, R, l),
        #         w
        #     )
        #     u = self.u_estimator.get_success_utility(task)
        #     # self.logger.log("{}, {}".format(task.priority.value, u))
        #     gain = \
        #         (p_good - p_bad) * u + \
        #         prev_gain

        #     self.suffix_sum_cache[index] = prev_gain
        #     prev_gain = gain

        # self.logger.log(" ")


class GreedySchedulingPolicy(SchedulingPolicy):

    def __init__(self):
        pass

    def initialize(self, direction, tasks, progress_info, schedule):
        self.queue = GreedySchedulingQueue(
            descending=(direction == FillDirection.EARLY))

    def get_next(self, date):
        if self.queue.is_empty():
            return None

        return self.queue.top()

    def get_max_session_duration(self):
        return math.inf

    def add(self, task_index, task, task_event):
        self.queue.add(
            task_event.task_index,
            (task_event.opposite_date,
             task.priority.value
             if task.priority is not None
             else math.inf)
        )

    def update(self, task_index, date, next_date, duration):
        pass

    def delete(self, task_index):
        self.queue.delete(task_index)


class PolynomialSchedulingPolicy(SchedulingPolicy):

    def __init__(self, utility_estimator):
        # A float. If 0, no randomness, always pick the highest score.
        self.random_power = 0
        self.default_urgency = INF_URGENCY
        self.u_estimator = utility_estimator

    def set_random_power(self, random_power):
        self.random_power = random_power

    def set_default_urgency(self, default_urgency):
        self.default_urgency = default_urgency

    def initialize(self, direction, tasks, progress_info, schedule):
        # only support direction = FillDirection.EARLY
        if direction != FillDirection.EARLY:
            raise ValueError(
                "PolynomialSchedulingPolicy only support direction = FillDirection.EARLY")

        self.tasks = tasks
        self.session_duration = 1

        self.valid_tasks = set()

        self.sampler = Sampler(seed=schedule.schedule_start.encode())

    def get_max_session_duration(self):
        return math.inf

    def set_date(self, date):
        self.days_from_today = Date.today().days_to(date)

    def get_score(self, task, days_from_today):
        # TODO: This assumes utility is positive, and the final score is lower the better.
        return task.get_urgency(days_from_today, self.default_urgency) / self.u_estimator.get_success_utility(task)

    def get_current_score(self, task):
        raise NotImplementedError()

    def get_next(self, date):
        if len(self.valid_tasks) == 0:
            return None

        best_score = math.inf
        best_task = None

        self.set_date(date)

        if self.random_power == 0:
            for valid_task in self.valid_tasks:
                score = self.get_score(
                    self.tasks[valid_task], self.days_from_today)
                if score < best_score:
                    best_score = score
                    best_task = valid_task

        else:
            valid_tasks_list = list(self.valid_tasks)
            scores = [
                self.get_score(self.tasks[valid_task], self.days_from_today)
                for valid_task in valid_tasks_list
            ]

            # if self.logger is not None:
            #     self.logger.log('------')
            #     self.logger.log(str(scores))

            # "normalize"

            for i in range(len(scores)):
                scores[i] = 1.0 / \
                    (1.0 + scores[i] / self.random_power)

            max_score = max(scores)
            if max_score > 0:
                for i in range(len(scores)):
                    scores[i] /= max_score

            # Now scores[i] is from 0 to 1, and the max element = 1

            scores = sorted(zip(scores, valid_tasks_list))

            # if self.logger is not None:
            #     self.logger.log(str(scores))

            pivot = min(util.lower_bound(scores, 0, len(scores),
                                         self.sampler.uniform(0, 1),
                                         comp=lambda a, b: a[0] < b),
                        len(scores) - 1)

            # if self.logger is not None:
            #     self.logger.log(str(pivot))

            best_task = self.sampler.choice(scores[pivot:])[1]

            # self.logger = None

        return best_task

    def add(self, task_index, task, task_event):
        self.valid_tasks.add(task_index)

    def update(self, task_index, date, next_date, duration):
        pass

    def delete(self, task_index):
        if task_index in self.valid_tasks:
            self.valid_tasks.remove(task_index)


class ARPSchedulingPolicy(SchedulingPolicy):

    def __init__(self, probability_estimator, utility_estimator):
        self.estimator = GreedyExpectedUtilityEstimator(
            probability_estimator,
            utility_estimator
        )

    def initialize(self, direction, tasks, progress_info, schedule):
        # only support direction = FillDirection.EARLY
        if direction != FillDirection.EARLY:
            raise ValueError(
                "ARPSchedulingPolicy only support direction = FillDirection.EARLY")

        self.max_session_duration = 1 * 60
        self.pressure_heuristic = 0.1
        self.estimator.set_logger(self.logger)
        self.estimator.initialize(
            tasks,
            progress_info,
            schedule,
            self.max_session_duration,
            self.pressure_heuristic
        )
        self.valid_tasks = set()

    def get_max_session_duration(self):
        return self.max_session_duration

    def get_next(self, date):
        if len(self.valid_tasks) == 0:
            return None

        best_gain = -math.inf
        best_task = None
        for valid_task in self.valid_tasks:
            gain = self.estimator.get_expected_utility_gain_per_duration(
                valid_task, self.max_session_duration)
            if gain > best_gain:
                best_gain = gain
                best_task = valid_task

        # self.logger.log("best_gain: {}".format(best_gain))

        return best_task

    def add(self, task_index, task, task_event):
        self.valid_tasks.add(task_index)

    def update(self, task_index, date, next_date, duration):
        self.estimator.update(task_index, next_date, duration)

    def delete(self, task_index):
        if task_index in self.valid_tasks:
            self.valid_tasks.remove(task_index)


class RLSchedulingPolicy(object):

    def __init__(self):
        pass

    def set_logger(self, logger):
        self.logger = logger

    def initialize(self, direction, tasks, progress_info, schedule):
        raise NotImplementedError()

    def get_max_session_duration(self):
        raise NotImplementedError()

    def get_next(self, date):
        raise NotImplementedError()

    def add(self, task_index, task, task_event):
        raise NotImplementedError()

    def update(self, task_index, date, next_date, duration):
        raise NotImplementedError()

    def delete(self, task_index):
        raise NotImplementedError()

    def train(self):
        import autograd.numpy as np
        from autograd import grad
        from autograd.misc.optimizers import adam

        NUM_TASKS = 10
        TASK_LENGTH_MEAN = 0.15
        TASK_LENGTH_STD = 0.2
        PERCENTAGE_STARTS_NOW = 0.5
        MIN_UTILITY = 1
        MAX_UTILITY = 10
        NUM_STEPS = 100

        def softmax(x):
            e_x = np.exp(x - np.max(x))
            return e_x / e_x.sum(axis=0)

        def get_initial_features(tasks):
            '''
            - amount left
            - time to deadline
            - utility

            note that they are column vectors.
            '''

            amount = np.array([
                amount
                for start, end, amount, utility in tasks
            ]).reshape((-1, 1))

            days_to_deadline = np.array([
                end
                for start, end, amount, utility in tasks
            ]).reshape((-1, 1))

            utility = np.array([
                utility
                for start, end, amount, utility in tasks
            ]).reshape((-1, 1))

            return amount, days_to_deadline, utility

        def update_features(old_features, scores, mask):
            old_amount, old_days_to_deadline, old_utility = old_features

            amount = old_amount + (mask.reshape((-1, 1))
                                   * scores.reshape((-1, 1)))
            days_to_deadline = old_days_to_deadline - 1
            utility = old_utility

            return amount, days_to_deadline, utility

        def generate_problem():
            tasks = []

            for i in range(NUM_TASKS):
                start = 0 if np.random.rand() < PERCENTAGE_STARTS_NOW else np.random.randint(
                    0, NUM_STEPS - 2)
                end = np.random.randint(0, NUM_STEPS - 1)
                if start > end:
                    start, end = end, start

                if end == start:
                    end = end + 1

                amount = np.random.normal(
                    TASK_LENGTH_MEAN, TASK_LENGTH_STD) * (end - start)
                if amount < 0:
                    amount = -amount

                utility = MIN_UTILITY + np.random.rand() * (MAX_UTILITY - MIN_UTILITY)

                tasks.append((start, end, amount, utility))

            task_points = []

            for i in range(len(tasks)):
                start, end, amount, utility = tasks[i]

                # time, is_end, task_index
                task_points.append((start, False, i))
                task_points.append((end + 1, True, i))

            task_points.sort()

            steps = []

            j = 0

            mask_list = [0 for _ in range(len(tasks))]

            for i in range(NUM_STEPS):
                loss_mask_list = [0 for _ in range(len(tasks))]

                while j < len(task_points) and task_points[j][0] <= i:
                    time, is_end, task_index = task_points[j]
                    if is_end:
                        mask_list[task_index] = 0
                        loss_mask_list[task_index] = 1

                    else:  # is start
                        mask_list[task_index] = 1

                    j += 1

                mask = np.array(mask_list)

                # use this before applying action
                utility_mask = np.array(loss_mask_list)

                steps.append((mask, utility_mask))

            initial_features = get_initial_features(tasks)

            return steps, initial_features

        def get_utility(initial_features, features, utility_mask):
            initial_amount = initial_features[0]
            amount, days_to_deadline, utility = features
            return np.sum(utility_mask * np.maximum(0, amount / initial_amount))

        def activation(z):
            return np.tanh(z) * 0.5 + 0.5

        def get_initial_params(layer_shapes):
            params = []
            for i in range(len(layer_shapes) - 1):
                W = np.random.normal(
                    0, 1 / layer_shapes[i], (layer_shapes[i], layer_shapes[i + 1]))
                b = np.random.normal(
                    0, 1 / layer_shapes[i], (layer_shapes[i + 1],))
                params.append((W, b))

            return params

        def model(data, params):
            output = data
            for W, b in params:
                output = activation(np.matmul(output, W) + b)

            # TODO in this implementation, last layer also have tanh
            return softmax(output)

        def get_total_utility(problem, params):
            total_utility = 0
            steps, initial_features = problem
            features = initial_features
            for mask, utility_mask in steps:
                utility = get_utility(initial_features, features, utility_mask)
                total_utility = total_utility + utility

                scores = model(np.concatenate(features, axis=1),
                               params).reshape((-1,))
                features = update_features(features, scores, mask)

            return total_utility

        NUM_PROBLEMS = 5
        NUM_ITERATIONS = 1000
        LAYER_SHAPES = [3, 8, 1]

        problems = [generate_problem() for _ in range(NUM_PROBLEMS)]
        problems *= NUM_ITERATIONS // NUM_PROBLEMS
        np.random.shuffle(problems)

        initial_params = get_initial_params(LAYER_SHAPES)

        def objective_function(params, iteration):
            problem = problems[iteration]
            return get_total_utility(problem, params)

        print(objective_function(initial_params, 0))

        PERCENTAGE_STARTS_NOW = 1

        reference_problem = generate_problem()

        def print_solution(problem, params):
            steps, initial_features = problem
            features = initial_features
            fake_score = np.zeros(features[0].shape[0])

            string = ""

            for i in range(features[0].shape[0]):
                print([feature[i, 0] for feature in features])

            task = 0
            for mask, utility_mask in steps:
                for i in range(utility_mask.shape[0]):
                    if utility_mask[i] > 0:
                        string += " {}] ".format(i)

                scores = model(np.concatenate(features, axis=1),
                               params).reshape((-1,))
                fake_score[task] = 0
                task = np.argmax(scores * mask)
                string += "{}".format(task)
                fake_score[task] = 1
                features = update_features(features, fake_score, mask)

            print(string)

        def optimizer_callback(params, iteration, gradient):
            print_solution(reference_problem, params)

        # TODO start with simple implementation (don't think about gradient).
        # then see if end-to-end works. if not, try manual (REINFORCE) gradient.

        # TODO mask the tasks when task already finished.

        # TODO other formulations

        objective_gradient = grad(objective_function)
        trained_params = adam(
            objective_gradient,
            initial_params,
            step_size=0.1,
            num_iters=NUM_ITERATIONS,
            callback=optimizer_callback
        )


# RLSchedulingPolicy().train()
