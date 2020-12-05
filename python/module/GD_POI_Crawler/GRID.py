

class Grid:
    def __init__(self, lbx, lby, rtx, rty):
        """
        Grid 是一个矩形类

        ------ty--------
        |              |
        lx            rx
        |              |
        ------by--------
        """
        self.lbx = lbx
        self.lby = lby
        self.rtx = rtx
        self.rty = rty
        self.set_steps(1, 1)
    
    def set_steps(self, steps_x, steps_y):
        """
        """
        self.steps_x = steps_x
        self.steps_y = steps_y	
        self.step_length_x = (self.rtx - self.lbx) / self.steps_x
        self.step_length_y = (self.rty - self.lby) / self.steps_y
    
    def get_str_bound(self, i, j):
        """
        116.292346,39.837445|116.48186,39.973077
        """
        b = self.get_float_bound(i, j)
        return "%s,%s|%s,%s" % (b['lbx'], b['lby'], b['rbx'], b['rby'])
    
    def get_float_bound(self, i, j):
        """
        """
        b = {}
        b['lbx'] = self.lbx + self.step_length_x * i
        b['lby'] = self.lby + self.step_length_y * j
        b['rbx'] = b['lbx'] + self.step_length_x
        b['rby'] = b['lby'] + self.step_length_y
        return b

    def get_bound(self, i, j):
        """
        """
        b = {}
        b['lbx'] = self.lbx + self.step_length_x * i
        b['lby'] = self.lby + self.step_length_y * j
        b['rbx'] = b['lbx'] + self.step_length_x
        b['rby'] = b['lby'] + self.step_length_y
        return Grid(b['lbx'], b['lby'], b['rbx'], b['rby'])
