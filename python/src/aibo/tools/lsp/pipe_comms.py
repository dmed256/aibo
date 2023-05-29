import os
import select
from threading import Thread

CLIENT_INDEX = 0
SERVER_INDEX = 1


class PipeComms:
    def __init__(self, *, log: bool = False):
        r1, w1 = os.pipe()
        r2, w2 = os.pipe()

        self._read_pipes = [r1, r2]
        self._write_pipes = [w1, w2]
        self._read_logs = ["", ""]

        self.is_logging = log
        if log:
            self._setup_logging()

    @property
    def client_read(self) -> int:
        return self._read_pipes[CLIENT_INDEX]

    @property
    def client_write(self) -> int:
        return self._write_pipes[CLIENT_INDEX]

    @property
    def client_logs(self) -> str:
        return self._read_logs[CLIENT_INDEX]

    @property
    def server_read(self) -> int:
        return self._read_pipes[SERVER_INDEX]

    @property
    def server_write(self) -> int:
        return self._write_pipes[SERVER_INDEX]

    @property
    def server_logs(self) -> str:
        return self._read_logs[SERVER_INDEX]

    def _setup_logging(self) -> None:
        # Without logging:
        #   [P1] w1 -> [P2] r1
        #   [P2] w2 -> [P1] r2
        #
        # With logging:
        #   [P1] w1 -> [L] r1 -> [L] log_w1 -> [P2] log_r1
        #   [P2] w2 -> [L] r1 -> [L] log_w2 -> [P1] log_r2
        log_r1, log_w1 = os.pipe()
        log_r2, log_w2 = os.pipe()

        r1, r2 = self._read_pipes
        self._read_pipes = [log_r1, log_r2]
        real_read_pipes = [r1, r2]

        logging_write_pipes = [log_w1, log_w2]

        def setup_pipe_logging() -> None:
            while True:
                ready_read_pipes, _, _ = select.select(real_read_pipes, [], [])

                for read_pipe in ready_read_pipes:
                    pipe_index = real_read_pipes.index(read_pipe)
                    data = os.read(read_pipe, 4096)
                    self._read_logs[pipe_index] += data.decode("utf-8")
                    os.write(logging_write_pipes[pipe_index], data)

        # Start the monitoring thread
        self._monitor_thread = Thread(target=setup_pipe_logging)
        self._monitor_thread.start()
