import math

from data_structure import *
from scheduling_util import *
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

    def get_next(self):
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
        
        if total_time == math.inf:
            return 1.0

        x = 0
        mean = (variable_time * self.mean_scale) - total_time
        std = abs(variable_time) * self.variable_time_std + abs(total_time) * self.total_time_std
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
        self.sorted_tasks.sort(key = lambda x : x[1].end)

        # compute link from index back to order
        self.index_to_order = [0 for _ in range(len(tasks))]
        for i in range(len(self.sorted_tasks)):
            self.index_to_order[self.sorted_tasks[i][0]] = i
    
        # initialization
        self.sorted_task_pressure = [0 for _ in range(len(self.sorted_tasks))]
        self.sorted_task_total_time = [0 for _ in range(len(self.sorted_tasks))]

        self.update()

    def get_expected_utility_gain_per_duration(self, task_index, duration):
        '''
        Duration is not used here because our estimate is independent of it.
        '''
        task = self.tasks[task_index]
        r = self.progress_info.get_amount_left(task_index)
        s = self.max_session_duration
        i = self.index_to_order[task_index]
        R = self.sorted_task_pressure[i]
        w = self.sorted_task_total_time[i] - s
        l = self.pressure_heuristic

        p_good = self.p_estimator.get_success_probability(
            task,
            lerp(r - s, R - s, l),
            w
        )

        p_bad = self.p_estimator.get_success_probability(
            task,
            lerp(r, R, l),
            w
        )

        u = self.u_estimator.get_success_utility(task)
        U = (p_good - p_bad) * u

        U += self.suffix_sum_cache[task_index]

        return U / self.max_session_duration

    def update(self, task_index = None, on_date = None, duration = None):
        '''
        Right now we simply re-read from reference to data passed in from initialize()
        '''
        # compute early schedule and total & variable time
        self.start_day = self.schedule.get_schedule_start().add_days(-1) # -1 is useful
        end_day = self.schedule.get_schedule_end()
        self.schedule_days = self.start_day.days_to(end_day)
        schedule_free_time_until = [0 for _ in range(self.schedule_days)]
        for i in range(1, self.schedule_days):
            schedule_free_time_until[i] = self.schedule.get_free_time(self.start_day.add_days(i)) + schedule_free_time_until[i - 1]

        prev_variable_time = 0
        for i in range(len(self.sorted_tasks)):
            variable_time = self.progress_info.get_amount_left(self.sorted_tasks[i][0]) + prev_variable_time
            self.sorted_task_pressure[i] = variable_time
            prev_variable_time = variable_time
            
        for i in range(len(self.sorted_tasks)):
            due = max(0, self.start_day.days_to(self.sorted_tasks[i][1].end))
            if due < self.schedule_days:
                self.sorted_task_total_time[i] = schedule_free_time_until[due]

            else:
                self.sorted_task_total_time[i] = math.inf

        # cache suffix sum
        self.suffix_sum_cache = [0 for _ in range(len(self.tasks))]
        prev_gain = 0
        for i in range(len(self.sorted_tasks) - 1, -1, -1):
            index, task = self.sorted_tasks[i]
            s = self.max_session_duration
            w = self.sorted_task_total_time[i] - s
            r = self.progress_info.get_amount_left(index)
            R = self.sorted_task_pressure[i]
            l = self.pressure_heuristic
            p_good = self.p_estimator.get_success_probability(
                task,
                lerp(r, R - s, l),
                w
            )
            p_bad = self.p_estimator.get_success_probability(
                task,
                lerp(r, R, l),
                w
            )
            u = self.u_estimator.get_success_utility(task)
            # self.logger.log("{}, {}".format(task.priority.value, u))
            gain = \
                (p_good - p_bad) * u + \
                prev_gain

            self.suffix_sum_cache[index] = prev_gain
            prev_gain = gain
            
        # self.logger.log(" ")


class GreedySchedulingPolicy(SchedulingPolicy):

    def __init__(self):
        pass

    def initialize(self, direction, tasks, progress_info, schedule):
        self.queue = GreedySchedulingQueue(descending = (direction == FillDirection.EARLY))

    def get_next(self):
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


class ARPSchedulingPolicy(SchedulingPolicy):

    def __init__(self, probability_estimator, utility_estimator):
        self.estimator = GreedyExpectedUtilityEstimator(
            probability_estimator,
            utility_estimator
        )

    def initialize(self, direction, tasks, progress_info, schedule):
        # only support direction = FillDirection.EARLY
        if direction != FillDirection.EARLY:
            raise ValueError("ARPSchedulingPolicy only support direction = FillDirection.EARLY")

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

    def get_next(self):
        if len(self.valid_tasks) == 0:
            return None

        best_gain = -math.inf
        best_task = None
        for valid_task in self.valid_tasks:
            gain = self.estimator.get_expected_utility_gain_per_duration(valid_task, self.max_session_duration)
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


class RLModel(object):

    def __init__(self):
        pass

    def get_score(self):
        raise NotImplementedError()

    def get_loss(self):
        raise NotImplementedError()


class RLSchedulingPolicy(object):

    def __init__(self):
        raise NotImplementedError()

    def set_logger(self, logger):
        self.logger = logger

    def initialize(self, direction, tasks, progress_info, schedule):
        raise NotImplementedError()

    def get_max_session_duration(self):
        raise NotImplementedError()

    def get_next(self):
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
        from autograd.misc.optimizer import adam

        NUM_TASKS = 100
        TASK_LENGTH_MEAN = 0.5
        TASK_LENGTH_STD = 1.0
        STEPS = 500

        def get_initial_features():
            raise NotImplementedError()

        def generate_problem():
            for i in range(NUM_TASKS):
                start = np.random.randint(0, STEPS - 1)
                end = np.random.randint(0, STEPS)
                if start > end:
                    start, end = end, start

                if end == start:
                    end = end + 1

                amount = max(0, np.random.normal(TASK_LENGTH_MEAN, TASK_LENGTH_STD) * (end - start))

            initial_features = get_initial_features()

            return steps, initial_features

        def update_features(old_features, action):
            raise NotImplementedError()

        def get_utility(features, loss_mask):
            raise NotImplementedError()

        def activation(z):
            return np.tanh(z) * 0.5 + 0.5
        
        def model(data, params):
            output = data
            for W, b in range(len(params)):
                output = activation(np.matmul(output, W) + b)

            # TODO in this implementation, last layer also have tanh
            return output

        def get_total_utility(problem, params):
            total_utility = 0
            steps, features = problem
            for step, mask, loss_mask in steps:
                scores = model(features, params)
                scores *= mask # remove ones that are not doable
                action = np.argmax(scores)
                features = update_features(features, action)
                utility = get_utility(features, loss_mask)
                total_utility = total_utility + utility

            return total_utility

        num_iterations = 1000
        problems = [generate_problem() for _ in num_iterations]
        
        def objective_function(params, iteration):
            problem = problems[iteration]
            return get_total_utility(problem, params)

        def optimizer_callback(params, iteration, gradient):
            raise NotImplementedError()

        # TODO start with simple implementation (don't think about gradient).
        # then see if end-to-end works. if not, try manual (REINFORCE) gradient.

        objective_gradient = grad(objective_function)
        trained_params = adam(
            objective_gradient,
            initial_params,
            step_size = 0.01,
            num_iters = num_iterations,
            callback = optimizer_callback
        )


